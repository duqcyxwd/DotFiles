/*jshint esversion: 6 */

const fs = require('fs');

var template = {
  title: '',
  rules: [
    {
      description: '',
      manipulators: [],
    },
  ],
};

function populateInitManipulator(k, to, mode) {
  var initKey = {
    type: 'basic',
    from: {
      simultaneous: [
        {
          key_code: mode.leadKey,
        },
        {
          key_code: k,
        },
      ],
      simultaneous_options: {
        key_down_order: 'strict',
        key_up_order: 'strict_inverse',
        to_after_key_up: [
          {
            set_variable: {
              name: mode.name,
              value: 0,
            },
          },
        ],
      },
      modifiers: {
        // optional: ['any'],
      },
    },
    to: [
      {
        set_variable: {
          name: mode.name,
          value: 1,
        },
      },
      {
        key_code: to.key_code,
        modifiers: to.modifiers,
      },
    ],
    parameters: {
      'basic.simultaneous_threshold_milliseconds': 500,
    },
  };
  return initKey;
}
function populateManipulator(k, to, mode) {
  var kTemplate = {
    type: 'basic',
    from: {
      key_code: k,
      modifiers: {
        // optional: ['any'],
      },
    },
    to: [
      {
        key_code: to.key_code,
        modifiers: to.modifiers,
      },
    ],
    conditions: [
      {
        type: 'variable_if',
        name: mode.name,
        value: 1,
      },
    ],
  };
  return kTemplate;
}

var populateSingleRule = (mode) => {
  let rule = {};
  rule.description = mode.description;

  let keys = Object.keys(mode.map);
  ms = keys.map((k) => populateManipulator(k, mode.map[k], mode));
  msi = keys.map((k) => populateInitManipulator(k, mode.map[k], mode));

  rule.manipulators = ms.concat(msi);

  return rule;
};

var errorFn = (err) => {
  // throws an error, you could also catch it here
  if (err) throw err;
  // success case, the file was saved
  console.log('File saved!');
};

var WKeyMode = {
  description: 'W: Universal Key for window shortcut',
  leadKey: 'w',
  name: 'W_Mode',
  map: {
    c: {
      key_code: 'c',
      modifiers: ['left_alt', 'left_control'],
    },
    d: {
      key_code: 'd',
      modifiers: ['left_alt', 'left_control'],
    },
    f: {
      key_code: 'f',
      modifiers: ['left_alt', 'left_control'],
    },
    h: {
      key_code: 'h',
      modifiers: ['left_alt', 'left_control'],
    },
    i: {
      key_code: 'i',
      modifiers: ['left_alt', 'left_control'],
    },
    j: {
      key_code: 'j',
      modifiers: ['left_alt', 'left_control'],
    },
    k: {
      key_code: 'k',
      modifiers: ['left_alt', 'left_control'],
    },
    l: {
      key_code: 'l',
      modifiers: ['left_alt', 'left_control'],
    },
    m: {
      key_code: 'm',
      modifiers: ['left_alt', 'left_control'],
    },
    o: {
      key_code: 'o',
      modifiers: ['left_alt', 'left_control'],
    },
    p: {
      key_code: 'p',
      modifiers: ['left_alt', 'left_control'],
    },
    t: {
      key_code: 't',
      modifiers: ['left_alt', 'left_control'],
    },
    y: {
      key_code: 'y',
      modifiers: ['left_alt', 'left_control'],
      note: 'Change main window',
    },
    spacebar: {
      key_code: 'spacebar',
      modifiers: ['left_control', 'left_alt'],
    },
    return_or_enter: {
      key_code: 'return_or_enter',
      modifiers: ['left_control', 'left_alt'],
    },
    left_arrow: {
      key_code: 'left_arrow',
      modifiers: ['left_control', 'left_alt'],
    },
    right_arrow: {
      key_code: 'right_arrow',
      modifiers: ['left_control', 'left_alt'],
    },
    up_arrow: {
      key_code: 'up_arrow',
      modifiers: ['left_control', 'left_alt'],
    },
    down_arrow: {
      key_code: 'down_arrow',
      modifiers: ['left_control', 'left_alt'],
    },
    comma: {
      key_code: 'comma',
      modifiers: ['left_control', 'left_alt'],
    },
    period: {
      key_code: 'period',
      modifiers: ['left_control', 'left_alt'],
    },
    delete_or_backspace: {
      key_code: 'delete_or_backspace',
      modifiers: ['left_control', 'left_alt'],
    },
    open_bracket: {
      key_code: 'open_bracket',
      modifiers: ['left_control', 'left_alt'],
    },
    close_bracket: {
      key_code: 'close_bracket',
      modifiers: ['left_control', 'left_alt'],
    },
    semicolon: {
      key_code: 'semicolon',
      modifiers: ['left_control', 'left_alt'],
    },
    quote: {
      key_code: 'quote',
      modifiers: ['left_control', 'left_alt'],
    },
    backslash: {
      key_code: 'backslash',
      modifiers: ['left_control', 'left_alt'],
    },
    1: {
      key_code: '1',
      modifiers: ['left_control', 'left_alt'],
    },
    0: {
      key_code: '0',
      modifiers: ['left_control', 'left_alt'],
    },
    equal_sign: {
      key_code: 'equal_sign',
      modifiers: ['left_control', 'left_alt'],
    },
    hyphen: {
      key_code: 'hyphen',
      modifiers: ['left_control', 'left_alt'],
    },
  },
};

var SKeyMode = {
  description: 'S: Enable directional key mode',
  leadKey: 's',
  name: 'S_Mode',
  map: {
    h: {
      key_code: 'left_arrow',
    },
    l: {
      key_code: 'right_arrow',
    },
    k: {
      key_code: 'up_arrow',
    },
    j: {
      key_code: 'down_arrow',
    },
  },
};

var ZKeyMode = {
  description: 'Z: Enable numberPad mode with',
  leadKey: 'z',
  name: 'Z_Mode',
  map: {
    m: {
      key_code: '1',
    },
    comma: {
      key_code: '2',
    },
    period: {
      key_code: '3',
    },
    j: {
      key_code: '4',
    },
    k: {
      key_code: '5',
    },
    l: {
      key_code: '6',
    },
    u: {
      key_code: '7',
    },
    i: {
      key_code: '8',
    },
    o: {
      key_code: '9',
    },
    spacebar: {
      key_code: 'keypad_0',
    },
  },
};

var custRule = {
  description: "User's cust rule",
  manipulators: [
    // {
    //   from: {
    //     key_code: 'left_control',
    //     modifiers: {
    //       optional: ['caps_lock'],
    //     },
    //   },
    //   parameters: {
    //     'basic.to_if_alone_timeout_milliseconds': 250,
    //     'basic.to_if_held_down_threshold_milliseconds': 250,
    //   },
    //   to_if_alone: [
    //     {
    //       key_code: 'escape',
    //     },
    //   ],
    //   to_if_held_down: [
    //     {
    //       shell_command: "open -a 'iTerm.app'",
    //     },
    //   ],
    //   type: 'basic',
    // },
    // Don't use, it cause typing delay
    //   {
    //     from: {
    //       key_code: 'i',
    //       modifiers: {
    //         optional: [],
    //       },
    //     },
    //     parameters: {
    //       'basic.to_if_alone_timeout_milliseconds': 250,
    //       'basic.to_if_held_down_threshold_milliseconds': 250,
    //     },
    //     to_if_alone: [
    //       {
    //         key_code: 'i',
    //       },
    //     ],
    //     to_if_held_down: [
    //       {
    //         shell_command: "open -a 'iTerm.app'",
    //       },
    //     ],
    //     type: 'basic',
    //   },
    //   {
    //     from: {
    //       key_code: 'i',
    //       modifiers: {
    //         mandatory: ['shift'],
    //       },
    //     },
    //     parameters: {
    //       'basic.to_if_alone_timeout_milliseconds': 250,
    //       'basic.to_if_held_down_threshold_milliseconds': 250,
    //     },
    //     to_if_alone: [
    //       {
    //         key_code: 'I',
    //       },
    //     ],
    //     to_if_held_down: [
    //       {
    //         key_code: 'i',
    //         modifiers: ['left_command', 'left_control'],
    //       },
    //     ],
    //     type: 'basic',
    //   },
  ],
};
function populateChuanPackage(modes) {
  let t = {
    title: '2020-10-16 Chuan Ergonomic mode',
    rules: [],
  };

  modes.forEach((mode) => {
    t.rules.push(populateSingleRule(mode));
  });

  // t.rules.push(custRule);

  return JSON.stringify(t, null, 2);
}
let file = '2020-10-16-chuan-package.json';
fs.writeFile(file, populateChuanPackage([SKeyMode, WKeyMode, ZKeyMode]), errorFn);

console.log(` rm ~/.config/karabiner/assets/complex_modifications/${file}`); // REMOVETAG
console.log(` cp ${file} ~/.config/karabiner/assets/complex_modifications`); // REMOVETAG

// {
//   "type": "basic",
//   "from": {
//     "key_code": "a",
//     "modifiers": {
//       "mandatory": [
//         "right_shift"
//       ],
//       "optional": [
//         "caps_lock"
//       ]
//     }
//   },
//   "to": [
//     {
//       "shell_command": "open '/Applications/Utilities/Activity Monitor.app'"
//     }
//   ]
// },

// {
//   "type": "basic",
//   "from": {
//       "key_code": "escape",
//       "modifiers": {
//           "optional": ["caps_lock"]
//       }
//   },
//   "parameters": {
//       "basic.to_if_alone_timeout_milliseconds": 250,
//       "basic.to_if_held_down_threshold_milliseconds": 250
//   },
//   "to_if_alone": [
//       {
//           "key_code": "escape"
//       }
//   ],
//   "to_if_held_down": [
//       {
//           "shell_command": "open -a 'Alfred 4.app'"
//       }
//   ]
// }
