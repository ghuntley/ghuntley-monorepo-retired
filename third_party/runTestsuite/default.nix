{ lib, pkgs, depot, ... }:

# Run a nix testsuite.
#
# The tests are simple assertions on the nix level,
# and can use derivation outputs if IfD is enabled.
#
# You build a testsuite by bundling assertions into
# “it”s and then bundling the “it”s into a testsuite.
#
# Running the testsuite will abort evaluation if
# any assertion fails.
#
# Example:
#
#   runTestsuite "myFancyTestsuite" [
#     (it "does an assertion" [
#       (assertEq "42 is equal to 42" "42" "42")
#       (assertEq "also 23" 23 23)
#     ])
#     (it "frmbls the brlbr" [
#       (assertEq true false)
#     ])
#   ]
#
# will fail the second it group because true is not false.

let
  inherit (depot.third_party.yants) sum struct string any unit defun list;

  # rewrite the builtins.partition result
  # to use `ok` and `err` instead of `right` and `wrong`.
  partitionTests = pred: xs:
    let res = builtins.partition pred xs;
    in {
      ok = res.right;
      err = res.wrong;
    };

  # The result of an assert,
  # either it’s true (yep) or false (nope).
  # If it’s nope, we return the left and right
  # side of the assert, together with the description.
  AssertResult =
    sum "AssertResult" {
      yep = struct "yep" {
        test = string;
      };
      nope = struct "nope" {
        test = string;
        left = any;
        right = any;
      };
    };

  # Result of an it. An it is a bunch of asserts
  # bundled up with a good description of what is tested.
  ItResult =
    struct "ItResult" {
      it-desc = string;
      asserts = list AssertResult;
    };

  # assert that left and right values are equal
  assertEq = defun [ string any any AssertResult ]
    (desc: left: right:
      if left == right
      then { yep = { test = desc; }; }
      else { nope = {
        test = desc;
        inherit left right;
      };
    });

  # Annotate a bunch of asserts with a descriptive name
  it = desc: asserts: {
    it-desc = desc;
    inherit asserts;
  };

  # Run a bunch of its and check whether all asserts are yep.
  # If not, abort evaluation with `throw`
  # and print the result of the test suite.
  #
  # Takes a test suite name as first argument.
  runTestsuite = defun [ string (list ItResult) unit ]
    (name: itResults:
      let
        goodAss = ass: {
          good = AssertResult.match ass {
            yep = _: true;
            nope = _: false;
          };
          x = ass;
        };
        goodIt = it: {
          inherit (it) it-desc;
          asserts = partitionTests (ass:
            AssertResult.match ass {
              yep = _: true;
              nope = _: false;
            }) it.asserts;
        };
        goodIts = partitionTests (it: (goodIt it).asserts.err == []);
        res = goodIts itResults;
      in
        if res.err == []
        then {}
        # TODO(Profpatsch): pretty printing of results
        # and probably also somewhat easier to read output
        else throw
          ( "testsuite ${name} failed!\n"
          + lib.generators.toPretty {} res));

in {
  inherit
    assertEq
    it
    runTestsuite
    ;
}
