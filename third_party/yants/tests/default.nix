{ depot, pkgs, ... }:

with builtins;
with depot.third_party.yants;

# Note: Derivations are not included in the tests below as they cause
# issues with deepSeq.

deepSeq rec {
  # Test that all primitive types match
  primitives = [
    (unit {})
    (int 15)
    (bool false)
    (float 13.37)
    (string "Hello!")
    (function (x: x * 2))
    (path /nix)
  ];

  # Test that polymorphic types work as intended
  poly = [
    (option int null)
    (list string [ "foo" "bar" ])
    (either int float 42)
  ];

  # Test that structures work as planned.
  person = struct "person" {
    name = string;
    age  = int;

    contact = option (struct {
      email = string;
      phone = option string;
    });
  };

  testPerson = person {
    name = "Brynhjulf";
    age  = 42;
    contact.email = "brynhjulf@yants.nix";
  };

  # Test enum definitions & matching
  colour = enum "colour" [ "red" "blue" "green" ];
  testMatch = colour.match "red" {
    red = "It is in fact red!";
    blue = throw "It should not be blue!";
    green = throw "It should not be green!";
  };

  # Test sum type definitions
  creature = sum "creature" {
    human = struct {
      name = string;
      age = option int;
    };

    pet = enum "pet" [ "dog" "lizard" "cat" ];
  };

  testSum = creature {
    human = {
      name = "Brynhjulf";
      age = 42;
    };
  };

  testSumMatch = creature.match testSum {
    human = v: "It's a human named ${v.name}";
    pet = v: throw "It's not supposed to be a pet!";
  };

  # Test curried function definitions
  func = defun [ string int string ]
  (name: age: "${name} is ${toString age} years old");

  testFunc = func "Brynhjulf" 42;

  # Test that all types are types.
  testTypes = map type [
    any bool drv float int string path

    (attrs int)
    (eitherN [ int string bool ])
    (either int string)
    (enum [ "foo" "bar" ])
    (list string)
    (option int)
    (option (list string))
    (struct { a = int; b = option string; })
    (sum { a = int; b = option string; })
  ];
} (pkgs.writeText "yants-tests" "All tests passed!")
