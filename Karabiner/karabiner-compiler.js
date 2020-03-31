console.log('Hi2'); // REMOVETAG

const fs = require('fs');

var template = {
  title: '',
  rules: [
    {
      description: '',
      manipulators: []
    }
  ]
};

function populateInitManipulator(k, to, mode) {
  var initKey = {
    type: 'basic',
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
        key_down_order: 'strict',
        key_up_order: 'strict_inverse',
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
        optional: ['any']
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
      'basic.simultaneous_threshold_milliseconds': 500
    }
  };
  return initKey;
}
function populateManipulator(k, to, mode) {
  var kTemplate = {
    type: 'basic',
    from: {
      key_code: k,
      modifiers: {
        optional: ['any']
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
        type: 'variable_if',
        name: mode.mode,
        value: 1
      }
    ]
  };
  return kTemplate;
}

var amethystMode = {
  mode: 'AmethystMode',
  description: 'Amethyst Mode All in one',
  leadKey: 'w',
  map: {
    c: {
      key_code: 'c',
      modifiers: ['left_alt', 'left_control']
    },
    d: {
      key_code: 'd',
      modifiers: ['left_alt', 'left_control']
    },
    f: {
      key_code: 'f',
      modifiers: ['left_alt', 'left_control']
    },
    h: {
      key_code: 'h',
      modifiers: ['left_alt', 'left_control']
    },
    i: {
      key_code: 'i',
      modifiers: ['left_alt', 'left_control']
    },
    j: {
      key_code: 'j',
      modifiers: ['left_alt', 'left_control']
    },
    k: {
      key_code: 'k',
      modifiers: ['left_alt', 'left_control']
    },
    l: {
      key_code: 'l',
      modifiers: ['left_alt', 'left_control']
    },
    m: {
      key_code: 'm',
      modifiers: ['left_alt', 'left_control']
    },
    o: {
      key_code: 'o',
      modifiers: ['left_alt', 'left_control']
    },
    p: {
      key_code: 'p',
      modifiers: ['left_alt', 'left_control']
    },
    t: {
      key_code: 't',
      modifiers: ['left_alt', 'left_control']
    },
    y: {
      key_code: 'y',
      modifiers: ['left_alt', 'left_control'],
      note: 'Change main window'
    },
    spacebar: {
      key_code: 'spacebar',
      modifiers: ['left_control', 'left_alt']
    },
    return_or_enter: {
      key_code: 'return_or_enter',
      modifiers: ['left_control', 'left_alt']
    },
    left_arrow: {
      key_code: 'left_arrow',
      modifiers: ['left_control', 'left_alt']
    },
    right_arrow: {
      key_code: 'right_arrow',
      modifiers: ['left_control', 'left_alt']
    },
    comma: {
      key_code: 'comma',
      modifiers: ['left_control', 'left_alt']
    },
    period: {
      key_code: 'period',
      modifiers: ['left_control', 'left_alt']
    },
    delete_or_backs: {
      key_code: 'delete_or_backs',
      modifiers: ['left_control', 'left_alt']
    },
    open_bracket: {
      key_code: 'open_bracket',
      modifiers: ['left_control', 'left_alt']
    },
    close_bracket: {
      key_code: 'close_bracket',
      modifiers: ['left_control', 'left_alt']
    },
    backslash: {
      key_code: 'backslash',
      modifiers: ['left_control', 'left_alt']
    },
    equal_sign: {
      key_code: 'equal_sign',
      modifiers: ['left_control', 'left_alt']
    },
    hyphen: {
      key_code: 'hyphen',
      modifiers: ['left_control', 'left_alt']
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

fs.writeFile('2020-03-Amethyst.json', result, err => {
  // throws an error, you could also catch it here
  if (err) throw err;

  // success case, the file was saved
  console.log('File saved!');
});
