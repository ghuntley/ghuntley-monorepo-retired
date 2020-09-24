# This file configures the primary build pipeline used for the
# top-level list of depot targets.
#
# It outputs a "YAML" (actually JSON) file which is evaluated and
# submitted to Buildkite at the start of each build. This means we can
# dynamically configure the pipeline execution here.
{ depot, lib, pkgs, ... }:

let
  inherit (builtins) concatStringsSep foldl' map toJSON;
  inherit (lib) singleton;
  inherit (pkgs) writeText;

  # Create an expression that builds the target at the specified
  # location.
  mkBuildExpr = target:
    let
      descend = expr: attr: "builtins.getAttr \"${attr}\" (${expr})";
      targetExpr = foldl' descend "import ./. {}" target.__readTree;
      subtargetExpr = descend targetExpr target.__subtarget;
    in if target ? __subtarget then subtargetExpr else targetExpr;

  # Create a pipeline label from the targets tree location.
  mkLabel = target:
    let label = concatStringsSep "/" target.__readTree;
    in if target ? __subtarget
      then "${label}:${target.__subtarget}"
      else label;

  # Create a pipeline step from a single target.
  #
  # If the build fails, Buildkite metadata is updated to mark the
  # pipeline as failed. Buildkite has a concept of a failed pipeline
  # regardless, but this data is not accessible.
  mkStep = target: {
    command = ''
      nix-build -E '${mkBuildExpr target}' --verbose || (buildkite-agent meta-data set "failure" "1"; exit 1)
    '';
    label = ":nix: ${mkLabel target}";
  };

  # This defines the build pipeline, using the pipeline format
  # documented on https://buildkite.com/docs/pipelines/defining-steps
  #
  # Pipeline steps need to stay in order.
  pipeline.steps =
    # Zero the failure status
    [
      {
        command = "buildkite-agent meta-data set 'failure' '0'";
        label = ":buildkite:";
      }
      { wait = null; }
    ]

    # Create build steps for each CI target
    ++ (map mkStep depot.ci.targets)

    ++ [
      # Wait for all previous checks to complete
      ({
        wait = null;
        continue_on_failure = true;
      })

      # Wait for all steps to complete, then exit with success or
      # failure depending on whether any failure status was written.
      # This step must be :duck:! (yes, really!)
      ({
        command = "exit $(buildkite-agent meta-data get 'failure')";
        label = ":duck:";
      })
    ];
in (writeText "depot.yaml" (toJSON pipeline))
