console.log("Hi2"); // REMOVETAG

const fs = require("fs");

var template = {
  title: "",
  rules: [
    {
      description: "",
      manipulators: []
    }
  ]
};

function populateInitManipulator(k, to, mode) {
  var initKey = {
    type: "basic",
    from: {
      simultaneous: [
        {
          key_code: mode.leadKey
        },
        {
          key_code: k
        }
      ],
      simultaneous_options: {
        key_down_order: "strict",
        key_up_order: "strict_inverse",
        to_after_key_up: [
          {
            set_variable: {
              name: mode.mode,
              value: 0
            }
          }
        ]
      },
      modifiers: {
        optional: ["any"]
      }
    },
    to: [
      {
        set_variable: {
          name: mode.mode,
          value: 1
        }
      },
      {
        key_code: to.key_code,
        modifiers: to.modifiers
      }
    ],
    parameters: {
      "basic.simultaneous_threshold_milliseconds": 500
    }
  };
  return initKey;
}
function populateManipulator(k, to, mode) {
  var kTemplate = {
    type: "basic",
    from: {
      key_code: k,
      modifiers: {
        optional: ["any"]
      }
    },
    to: [
      {
        key_code: to.key_code,
        modifiers: to.modifiers
      }
    ],
    conditions: [
      {
        type: "variable_if",
        name: mode.mode,
        value: 1
      }
    ]
  };
  return kTemplate;
}

var amethystMode = {
  mode: "AmethystMode",
  description: "Amethyst Mode All in one",
  leadKey: "w",
  map: {
    y: {
      key_code: "y",
      modifiers: ["left_control", "left_alt"]
    },
    p: {
      key_code: "p",
      modifiers: ["left_control", "left_alt"]
    },
    j: {
      key_code: "j",
      modifiers: ["left_control", "left_alt"]
    },
    k: {
      key_code: "k",
      modifiers: ["left_control", "left_alt"]
    },
    h: {
      key_code: "h",
      modifiers: ["left_control", "left_alt"]
    },
    l: {
      key_code: "l",
      modifiers: ["left_control", "left_alt"]
    },
    f: {
      key_code: "f",
      modifiers: ["left_control", "left_alt"]
    },
    t: {
      key_code: "t",
      modifiers: ["left_control", "left_alt"]
    },
    spacebar: {
      key_code: "spacebar",
      modifiers: ["left_control", "left_alt"]
    },
    return_or_enter: {
      key_code: "return_or_enter",
      modifiers: ["left_control", "left_alt"]
    },
    left_arrow: {
      key_code: "left_arrow",
      modifiers: ["left_control", "left_alt"]
    },
    right_arrow: {
      key_code: "right_arrow",
      modifiers: ["left_control", "left_alt"]
    },
    comma: {
      key_code: "comma",
      modifiers: ["left_control", "left_alt"]
    },
    delete_or_backs: {
      key_code: "delete_or_backs",
      modifiers: ["left_control", "left_alt"]
    },
    c: {
      key_code: "c",
      modifiers: ["left_control", "left_alt"]
    }
  }
};

function populateConfig(mode) {
  let t = template;
  t.title = mode.mode;
  t.rules[0].description = mode.description;

  let keys = Object.keys(mode.map);
  ms = keys.map(k => populateManipulator(k, mode.map[k], mode));
  msi = keys.map(k => populateInitManipulator(k, mode.map[k], mode));

  t.rules[0].manipulators = ms.concat(msi);

  return JSON.stringify(t, null, 2);
}

var result = populateConfig(amethystMode);

fs.writeFile("2019-10-18-Amethyst.json", result, err => {
  // throws an error, you could also catch it here
  if (err) throw err;

  // success case, the file was saved
  console.log("File saved!");
});
