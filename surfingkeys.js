/*jshint esversion: 9 */
/* jshint -W087 */

//     _____             __ _             _  __                 _____      _   _   _
//    / ____|           / _(_)           | |/ /                / ____|    | | | | (_)
//   | (___  _   _ _ __| |_ _ _ __   __ _| ' / ___ _   _ ___  | (___   ___| |_| |_ _ _ __   __ _ ___
//    \___ \| | | | '__|  _| | '_ \ / _` |  < / _ \ | | / __|  \___ \ / _ \ __| __| | '_ \ / _` / __|
//    ____) | |_| | |  | | | | | | | (_| | . \  __/ |_| \__ \  ____) |  __/ |_| |_| | | | | (_| \__ \
//   |_____/ \__,_|_|  |_| |_|_| |_|\__, |_|\_\___|\__, |___/ |_____/ \___|\__|\__|_|_| |_|\__, |___/
//                                   __/ |          __/ |                                   __/ |
//                                  |___/          |___/                                   |___/
//
//    Chuan's super awesome suring key setting

var cleanAlias = function () {
  removeSearchAliasX('g');
  removeSearchAliasX('d');
  removeSearchAliasX('b');
  removeSearchAliasX('w');
  removeSearchAliasX('s');
  removeSearchAliasX('h');
  removeSearchAliasX('y');
};
// unmapAllExcept();
// cleanAlias();

// Section: inline Query {{{1
//  --------------------------------------------------------------------------
let youdao = () =>
  Front.registerInlineQuery({
    url: function (q) {
      return `http://dict.youdao.com/w/eng/${q}/#keyfrom=dict2.index`;
    },
    parseResult: function (res) {
      var parser = new DOMParser();
      var doc = parser.parseFromString(res.text, 'text/html');
      var collinsResult = doc.querySelector('#collinsResult');
      var authTransToggle = doc.querySelector('#authTransToggle');
      var examplesToggle = doc.querySelector('#examplesToggle');
      if (collinsResult) {
        collinsResult.querySelectorAll('div>span.collinsOrder').forEach(function (span) {
          span.nextElementSibling.prepend(span);
        });
        collinsResult.querySelectorAll('div.examples').forEach(function (div) {
          div.innerHTML = div.innerHTML.replace(/<p/gi, '<span').replace(/<\/p>/gi, '</span>');
        });
        var exp = collinsResult.innerHTML;
        return exp;
      } else if (authTransToggle) {
        authTransToggle.querySelector('div.via.ar').remove();
        return authTransToggle.innerHTML;
      } else if (examplesToggle) {
        return examplesToggle.innerHTML;
      }
    },
  });

let shanbay = () =>
  Front.registerInlineQuery({
    url: 'https://api.shanbay.com/bdc/search/?word=',
    parseResult: function (res) {
      try {
        res = JSON.parse(res.text);
        var exp = res.msg;
        if (res.data.definition) {
          var pronunciations = [];
          for (var reg in res.data.pronunciations) {
            pronunciations.push(`<div>[${reg}] ${res.data.pronunciations[reg]}</div>`);
            // pronunciations.push(`<div><audio src="${res.data[reg+'_audio']}" controls></audio></div>`);
          }
          var definition = res.data.definition
            .split('\n')
            .map(function (d) {
              return `<li>${d}</li>`;
            })
            .join('');
          exp = `${pronunciations.join('')}<ul>${definition}</ul>`;
        }
        if (res.data.en_definitions) {
          exp += '<hr/>';
          for (var lex in res.data.en_definitions) {
            var sense = res.data.en_definitions[lex]
              .map(function (s) {
                return `<li>${s}</li>`;
              })
              .join('');
            exp += `<div>${lex}</div><ul>${sense}</ul>`;
          }
        }
        return exp;
      } catch (e) {
        return '';
      }
    },
  });

youdao();
// shanbay();

// Section: Basic setting {{{1
//  --------------------------------------------------------------------------
Hints.characters = 'asdfgqwertvbn';
Hints.numericHints = false;
Hints.filterHints = false;

settings.richHintsForKeystroke = 300;
settings.hintAlign = 'left';
settings.defaultSearchEngine = 'L';
settings.scrollStepSize = 130;
settings.smoothScroll = true;
// }}}
// Section: Help functions {{{1
//  --------------------------------------------------------------------------

function getElementsByText(str, tag = 'a') {
  return Array.prototype.slice.call(document.getElementsByTagName(tag)).filter((el) => el.textContent.includes(str.trim()));
}

function copytext(text) {
  var textField = document.createElement('textarea');
  textField.innerText = text;
  document.body.appendChild(textField);
  textField.select();
  document.execCommand('copy');
  textField.remove();
}

// elementClick
let eClick = (selectors) => () => document.querySelectorAll(selectors).forEach((e) => e.click());
let tClick = (s, tag = 'a') => () => getElementsByText(s, tag)[0].click();

// }}}
// Section: Key mapping {{{1
//  --------------------------------------------------------------------------

mapkey('gE', '#12 go Extensions - Open Chrome extensions Shortcut setting', function () {
  tabOpenLink('chrome://extensions/shortcuts');
});

mapkey('yA', '#7Copy all tabs url', function () {
  //get numbers of tabs
  chrome.tabs.query(
    {
      windowType: 'normal',
    },
    function (tabs) {
      tabNums = tabs.length;
    }
  );
  Front.showPopup('tabNums'); //debug

  var URLsToYank = [];
  chrome.tabs.query(
    {
      active: true,
      lastFocusedWindow: true,
    },
    function (tabs) {
      for (i = 0; i < tabNums; i++) var url = tabs[i].url;
      URLsToYank.push(url);
    }
  )();
  Clipboard.write(URLsToYank);
});

// # Insert your preferred key mappings here.
// map S scrollFullPageUp
mapkey('gp', '#3pin/unpin current tab', function () {
  RUNTIME('togglePinTab');
});
map('J', '<<');
map('K', '>>');
map('h', 'E');
map('l', 'R');
map('H', 'S');
map('L', 'D');
map('s', 'u');
map('<Ctrl-z>', '<Alt-s>');
map('<Ctrl-i>', '<Alt-s>');

//  --------------------------------------------------------------------------
// }}}
// Section: Search {{{1
//  --------------------------------------------------------------------------

addSearchAliasX('L', 'Im feeling lucky', 'https://www.google.com/search?btnI=1&q=');
// Section: Language {{{2
//  --------------------------------------------------------------------------
//language --
//
// addSearchAliasX(
//   "lJ",
//   "language Javascript",
//   "https://www.google.com/search?q=Javascript+"
// );
// addSearchAliasX("lj", "language java", "https://www.google.com/search?q=Java+");
// //addSearchAliasX('lC', 'C++', 'https://www.google.com/search?q=C++');
// addSearchAliasX(
//   "lc",
//   "language c",
//   "https://www.google.com/search?q=c+language+"
// );
// addSearchAliasX("l#", "language C#", "https://www.google.com/search?q=c%23+");
// addSearchAliasX("lR", "language R", "https://www.google.com/search?q=languag+");
// addSearchAliasX("lr", "language Ruby", "https://www.google.com/search?q=Ruby+");
// addSearchAliasX(
//   "lP",
//   "language Python",
//   "https://www.google.com/search?q=Python+"
// );
// addSearchAliasX("lp", "language php", "https://www.google.com/search?q=php+");
// addSearchAliasX(
//   "lK",
//   "language Kotlin",
//   "https://www.google.com/search?q=Kotlin+"
// );
// addSearchAliasX(
//   "lS",
//   "language Swift",
//   "https://www.google.com/search?q=Swift+"
// );
// addSearchAliasX(
//   "lQ",
//   "language SQL Query",
//   "https://www.google.com/search?q=SQL+"
// );
// addSearchAliasX(
//   "ls",
//   "language Shell script",
//   "https://www.google.com/search?q=Shell+Schript+"
// );
// addSearchAliasX(
//   "lT",
//   "language Typescript",
//   "https://www.google.com/search?q=TypeScript+"
// );
// addSearchAliasX("lH", "language HTML", "https://www.google.com/search?q=HTML+");
//
// }}}

//  --------------------------------------------------------------------------
// }}}
// Section: Domain/Site specific command {{{1
//  --------------------------------------------------------------------------

// Section: Atlassian JIRA {{{2
//  --------------------------------------------------------------------------
const atlassianJIRAMapping = () => {
  let domainMap = {
      domain: /(cenx.*|jira-.*)/i,
    },
    jstate = { sideHidden: true };

  mapkey(
    ',zm',
    'Toggle navigation bar',
    function () {
      document.querySelectorAll('button[aria-label="Toggle navigation"]')[0].click();
    },
    domainMap
  );
  mapkey(
    ',,y',
    'Copy JIRA TO MarkDown',
    function () {
      // let header = document.querySelectorAll('h1[data-test-id="issue.views.issue-base.foundation.summary.heading"]')[0].textContent,
      let header = document.querySelectorAll('.editable-field')[0].textContent;
      (url = window.location.href), (cd = url.match(/CD-[0-9]*/)[0]);

      let type = document.querySelectorAll('#issuedetails .item #type-val')[0],
        typeText = '';

      if (type) {
        typeText = type.textContent.replace(/\n/g, '').replace(/ /g, '');

        switch (typeText) {
          case 'Requirement':
            cd = '🟠' + ' [' + 'R:' + cd + ']';
            break;
          case 'Epic':
            cd = '🟣' + ' [' + 'E:' + cd + ']';
            break;
          case 'Story':
            cd = '🟢' + ' [' + 'S:' + cd + ']';
            break;
          case 'Bug':
            cd = '🔴' + ' [' + 'BB:' + cd + ']';

            break;
          default:
          // code block
        }

        typeText = typeText + ': ';
      }

      md = cd + '(' + url + ')' + ' ' + typeText + header;

      console.log(typeText);
      console.log(md);
      Clipboard.write(md);
    },
    domainMap
  );

  mapkey(
    ',,c',
    'Clean view',
    function () {
      let d = document.getElementById('viewissuesidebar');
      let dlist = document.querySelectorAll('.aui-item.list-results-panel')[0];

      if (jstate.sideHidden) {
        d.style.width = '0%';
        if (dlist != null) dlist.style.display = 'none';
      } else {
        d.style.width = '30%';
        if (dlist != null) dlist.style.display = '';
      }
      // document.querySelectorAll('.aui-item.list-results-panel')[0].style.width="250px"
      jstate.sideHidden = !jstate.sideHidden;
    },
    domainMap
  );

  mapkey(
    ',pr',
    'Open pull request',
    function () {
      getElementsByText('pull request', (tag = 'span'))[0].click();
    },
    domainMap
  );
};

// Section: Github {{{2
//  --------------------------------------------------------------------------
// github default shortcut lists
// https://help.github.com/articles/using-keyboard-shortcuts/
const githubMapping = () => {
  mapkey(
    ',yg',
    '#7 git clone',
    function () {
      Clipboard.write('git clone ' + window.location.href + '.git');
    },
    {
      domain: /github\.com/i,
    }
  );

  mapkey(
    ',gC',
    'Go to the code tab',
    function () {
      document.querySelectorAll('.js-selected-navigation-item.reponav-item')[0].click();
    },
    {
      domain: /github\.com/i,
    }
  );

  mapkey(
    ',gI',
    'Go to the Issues tab. ',
    function () {
      document.querySelectorAll('.js-selected-navigation-item.reponav-item')[1].click();
    },
    {
      domain: /github\.com/i,
    }
  );

  mapkey(
    ',gP',
    'Go to the Pull requests tab.  ',
    function () {
      document.querySelectorAll('.js-selected-navigation-item.reponav-item')[2].click();
    },
    {
      domain: /github\.com/i,
    }
  );
  mapkey(
    ',gB',
    'Go to the Projects tab. "',
    function () {
      document.querySelectorAll('.js-selected-navigation-item.reponav-item')[3].click();
    },
    {
      domain: /github\.com/i,
    }
  );

  mapkey(
    ',gW',
    'Go to the Wiki tab. ',
    function () {
      document.querySelectorAll('.js-selected-navigation-item.reponav-item')[4].click();
    },
    {
      domain: /github\.com/i,
    }
  );

  mapkey(
    ',gO',
    'Go to the Overview tab. ',
    function () {
      document.querySelectorAll('.UnderlineNav-item')[0].click();
    },
    {
      domain: /github\.com/i,
    }
  );
  mapkey(
    ',gR',
    'Go to the Repository tab. ',
    function () {
      document.querySelectorAll('.UnderlineNav-item')[1].click();
    },
    {
      domain: /github\.com/i,
    }
  );
  mapkey(
    ',gS',
    'Go to the Stars tab. ',
    function () {
      document.querySelectorAll('.UnderlineNav-item')[2].click();
    },
    {
      domain: /github\.com/i,
    }
  );
};
// ==========================
// }}}
// Section: Gitlab {{{2
//  --------------------------------------------------------------------------

var state = { scheduleHidden: true };

const gitlabMapping = () => {
  const gitlabDomain = {
    domain: /gitlab/i,
  };

  const gitlabOneLevelUp = () => {
    let repoTree = document.querySelectorAll('.breadcrumb.repo-breadcrumb .breadcrumb-item>a');
    console.log(repoTree);
    if (repoTree.length > 0) {
      repoTree[repoTree.length - 2].click();
    }
  };

  const gitlabToggleSidebar = () => {
    document.querySelectorAll('.rspec-toggle-sidebar').forEach((e) => e.click());
  };

  const cleanSchedule = () => {
    console.log('github clean');
    let rows = document.querySelectorAll('.pipeline-schedule-table-row');
    rows.forEach((r) => {
      if (!r.children[4].children[1].text.includes('Yongqinchuan')) {
        r.hidden = state.scheduleHidden;
      }
    });

    let pipeline = document.querySelectorAll('.ci-table .commit');
    pipeline.forEach((r) => {
      if (!String(r.children[2].children[0].href).includes('yongqinchuan')) {
        r.hidden = state.scheduleHidden;
      }
    });

    state.scheduleHidden = !state.scheduleHidden;
  };

  const gitlabCopyUrl = () => {
    let mrTitle = document.querySelectorAll('.title.qa-title')[0],
      knownPathRegex = /(merge_requests\/\d*|pipelines\/\d*|jobs\/\d*)/,
      url = window.location.href,
      path = window.location.pathname,
      titleText,
      linkName,
      shortPath,
      shortPathText;

    titleText = mrTitle ? mrTitle.textContent.replace(/\n/g, '') : '';

    knownPath = url.match(knownPathRegex);

    if (knownPath) {
      linkName = knownPath[0]
        .replace(/merge_requests\//, 'MR: #')
        .replace(/pipelines\//, 'Pipeline: #')
        .replace(/jobs\//, 'Job: #');
    } else {
      // shortPath = path.match(/\/\w+\/\w+\/?$/);
      shortPath = path.match(/\/\w+\/[a-zA-Z0-9-_]+\/?$/);
      linkName = shortPath ? shortPath[0].replace(/^\//, '') : '';
    }

    md = '[' + linkName + '](' + url + ')  ' + titleText;

    console.log(md);
    Clipboard.write(md);
  };

  mapkey(',,y', 'Gitlab: copy mr/pipeline link', gitlabCopyUrl, gitlabDomain);
  mapkey(',,c', 'Gitlab: github clean', cleanSchedule, gitlabDomain);
  mapkey(',,u', 'Gitlab: go one level up', gitlabOneLevelUp, gitlabDomain);
  mapkey('\\s', 'Gitlab: toggle sidebar', gitlabToggleSidebar, gitlabDomain);
  mapkey(',,s', 'Gitlab: go to schedule', tClick('Schedules'), gitlabDomain);
  mapkey(',,m', 'Gitlab: go to mr', eClick('.shortcuts-merge_requests'), gitlabDomain);
  mapkey(',,p', 'Gitlab: go to pipeline', eClick('.shortcuts-pipelines'), gitlabDomain);
  mapkey(',,f', 'Gitlab: go to Files', tClick('Files'), gitlabDomain);
  mapkey(',,g', 'Gitlab: go to Find file', tClick('Find file'), gitlabDomain);
  mapkey(',,zm', 'Gitlab: fold all diff', eClick('.file-title'), gitlabDomain);
  // mapkey(',,f', 'Gitlab: go to search', () => window.location('/cenx/cenx/-/find_file/develop'), gitlabDomain);
  // https://gitlab.rosetta.ericssondevops.com/cenx/cenx/-/find_file/develop
};

// Section: Google Search Result {{{2
//  --------------------------------------------------------------------------

const googleMapping = () => {
  const googleDomain = {
    domain: /google/i,
  };

  const state = { adHidden: true };

  const cleanAd = () => {
    let rows = document.querySelectorAll('.srg .g');
    rows.forEach((r) => {
      r.hidden = state.adHidden;
    });

    state.adHidden = !state.adHidden;
  };

  mapkey(',,c', 'Google: Ad clean', cleanAd, googleDomain);
  mapkey(',,,', 'Google: Ad clean', cleanAd, googleDomain);
  // mapkey(',,a', 'Google: Ad clean', cleanAd);
};

//  --------------------------------------------------------------------------
// }}}
// Section: Dev {{{2
//  --------------------------------------------------------------------------

const devMapping = () => {
  let devDomain = {
    domain: /(localnet|localhost|8080|\/client\/)/i,
  };
  mapkey(
    ',,r',
    'Related',
    function () {
      document.querySelectorAll('.network-list .network-view-card')[0].click();
    },
    devDomain
  );
  mapkey(
    ',,f',
    'Select first',
    function () {
      document.querySelectorAll('.network-view-card')[0].click();
    },
    devDomain
  );

  mapkey(
    ',,c',
    'Clean all notification',
    function () {
      document.querySelectorAll('.notifications-tc .notification').forEach((e) => e.click());
    },
    devDomain
  );

  mapkey(
    ',,b',
    'Net Back',
    function () {
      document.querySelectorAll('.network-back-btn').forEach((e) => e.click());
    },
    devDomain
  );

  mapkey(
    ',,y',
    'dev: copy name',
    function () {
      name = document.querySelectorAll('.header.navbar-header')[0].textContent;
      Clipboard.write(name);
    },
    devDomain
  );

  mapkey(
    ',,d',
    'debug',
    () => {
      debugger;
    },
    {}
  );
};

githubMapping();
atlassianJIRAMapping();
gitlabMapping();
devMapping();
googleMapping();
//  --------------------------------------------------------------------------
// }}}
//  --------------------------------------------------------------------------
// }}}
// Section: Styles {{{1
//  --------------------------------------------------------------------------

// Styles from : https://gist.github.com/emraher/2c071182ce0f04f3c69f6680de335029#file-surfingkeysdraculathemeattempt-txt-l1
// -----------------------------------------------------------------------------------------------------------------------
// Change hints styles
// -----------------------------------------------------------------------------------------------------------------------
// Hints.style('border: solid 1px #ff79c6; color:#44475a; background: #f1fa8c; background-color: #f1fa8c; font-size: 10pt; font-family: "Fira Code"');
// Mine
Hints.style(
  'color: #e2e3e7; font-size: 10px; text-shadow: none; background: #383c4a; border: 0.25em solid #383c4a; border-radius: 0.68em; box-shadow: 0em 0.1em 0.6em 0.1em rgba(0, 0, 0, 0.4); '
);
Hints.style(
  'border: solid 2px #ff79c6;padding: 1px; color: #e2e3e7; font-size: 10px; text-shadow: none; background: #808596; border: 0.25em solid #383c4a; border-radius: 0.68em; box-shadow: 0em 0.1em 0.6em 0.1em rgba(0, 0, 0, 0.4); ',
  'text'
);

// -----------------------------------------------------------------------------------------------------------------------
// Change search marks and cursor
// -----------------------------------------------------------------------------------------------------------------------
Visual.style('marks', 'background-color: #f1fa8c;');
// Visual.style('marks', 'color: #; font-size: 10px; text-shadow: none; background: #383c4a; border: 0.25em solid #383c4a; border-radius: 0.68em; box-shadow: 0em 0.1em 0.6em 0.1em rgba(0, 0, 0, 0.4); ');
Visual.style('cursor', 'background-color: #6272a4; color: #f8f8f2');

// Section: Theme {{{1
//  --------------------------------------------------------------------------

settings.Notheme = `
.sk_theme input {
    font-family: "Fira Code";
}
.sk_theme .url {
    font-size: 10px;
}
#sk_omnibarSearchResult li div.url {
    font-weight: normal;
}
.sk_theme .omnibar_timestamp {
    font-size: 11px;
    font-weight: bold;
}
.sk_theme .omnibar_visitcount {
    font-size: 11px;
    font-weight: bold;
}
body {
    font-family: "Fira Code", Consolas, "Liberation Mono", Menlo, Courier, monospace;
    font-size: 14px;
}
kbd {
    font: 11px "Fira Code", Consolas, "Liberation Mono", Menlo, Courier, monospace;
}
#sk_omnibarSearchArea .prompt, #sk_omnibarSearchArea .resultPage {
    font-size: 12px;
}
.sk_theme {
    background: #282a36;
    color: #f8f8f2;
}
.sk_theme tbody {
    color: #ff5555;
}
.sk_theme input {
    color: #ffb86c;
}
.sk_theme .url {
    color: #6272a4;
}
#sk_omnibarSearchResult>ul>li {
    background: #282a36;
}
#sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #282a36;
}
.sk_theme .annotation {
    color: #6272a4;
}
.sk_theme .focused {
    background: #44475a !important;
}
.sk_theme kbd {
    background: #f8f8f2;
    color: #44475a;
}
.sk_theme .frame {
    background: #8178DE9E;
}
.sk_theme .omnibar_highlight {
    color: #8be9fd;
}
.sk_theme .omnibar_folder {
    color: #ff79c6;
}
.sk_theme .omnibar_timestamp {
    color: #bd93f9;
}
.sk_theme .omnibar_visitcount {
    color: #f1fa8c;
}
.sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #282a36;
}
.sk_theme .prompt, .sk_theme .resultPage {
    color: #50fa7b;
}
.sk_theme .feature_name {
    color: #ff5555;
}
.sk_omnibar_middle #sk_omnibarSearchArea {
    border-bottom: 1px solid #282a36;
}
#sk_status {
    border: 1px solid #282a36;
}
#sk_richKeystroke {
    background: #282a36;
    box-shadow: 0px 2px 10px rgba(40, 42, 54, 0.8);
}
#sk_richKeystroke kbd>.candidates {
    color: #ff5555;
}
#sk_keystroke {
    background-color: #282a36;
    color: #f8f8f2;
}
kbd {
    border: solid 1px #f8f8f2;
    border-bottom-color: #f8f8f2;
    box-shadow: inset 0 -1px 0 #f8f8f2;
}
#sk_frame {
    border: 4px solid #ff5555;
    background: #8178DE9E;
    box-shadow: 0px 0px 10px #DA3C0DCC;
}
#sk_banner {
    border: 1px solid #282a36;
    background: rgb(68, 71, 90);
}
div.sk_tabs_bg {
    background: #f8f8f2;
}
div.sk_tab {
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#6272a4), color-stop(100%,#44475a));
}
div.sk_tab_title {
    color: #f8f8f2;
}
div.sk_tab_url {
    color: #8be9fd;
}
div.sk_tab_hint {
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#f1fa8c), color-stop(100%,#ffb86c));
    color: #282a36;
    border: solid 1px #282a36;
}
#sk_bubble {
    border: 1px solid #f8f8f2;
    color: #282a36;
    background-color: #f8f8f2;
}
#sk_bubble * {
    color: #282a36 !important;
}
div.sk_arrow[dir=down]>div:nth-of-type(1) {
    border-top: 12px solid #f8f8f2;
}
div.sk_arrow[dir=up]>div:nth-of-type(1) {
    border-bottom: 12px solid #f8f8f2;
}
div.sk_arrow[dir=down]>div:nth-of-type(2) {
    border-top: 10px solid #f8f8f2;
}
div.sk_arrow[dir=up]>div:nth-of-type(2) {
    border-bottom: 10px solid #f8f8f2;
}
}`;

// }}}
// Section: Define default settings {{{1
//  --------------------------------------------------------------------------

// Rewrite default Keybindings
mapkey('?', '#0Show usage', function () {
  Front.showUsage();
});
mapkey('t', '#8Open a URL', function () {
  Front.openOmnibar({ type: 'URLs', extra: 'getAllSites' });
});
mapkey(':', '#8Open commands', function () {
  Front.openOmnibar({ type: 'Commands' });
});

// Google
if (window.navigator.userAgent.indexOf('Firefox') <= 0) {
  mapkey(',cn', '#3Open newtab', function () {
    tabOpenLink('chrome://newtab/');
  });
  mapkey(',ca', '#12Open Chrome About', function () {
    tabOpenLink('chrome://help/');
  });
  mapkey(',cb', '#12Open Chrome Bookmarks', function () {
    tabOpenLink('chrome://bookmarks/');
  });
  mapkey(',cc', '#12Open Chrome Cache', function () {
    tabOpenLink('chrome://cache/');
  });
  mapkey(',cd', '#12Open Chrome Downloads', function () {
    tabOpenLink('chrome://downloads/');
  });
  mapkey(',ch', '#12Open Chrome History2', function () {
    tabOpenLink('chrome://history/');
  });
  mapkey(',ck', '#12Open Chrome Cookies', function () {
    tabOpenLink('chrome://settings/content/cookies');
  });
  mapkey(',ce', '#12Open Chrome Extensions', function () {
    tabOpenLink('chrome://extensions/');
  });
  mapkey(',cn', '#12Open Chrome net-internals', function () {
    tabOpenLink('chrome://net-internals/#proxy');
  });
  mapkey(',ci', '#12Open Chrome Inspect2', function () {
    tabOpenLink('chrome://inspect/#devices');
  });
}

mapkey(',fn', '#1Open a link in new tab', function () {
  Hints.create('', Hints.dispatchMouseClick, { tabbed: true });
});

// <TODO> in no filter mode, we can open in current page</TODO>
mapkey(',ff', '#1Open a link, press SHIFT to flip overlapped hints, hold SPACE to hide hints', function () {
  Hints.create('', Hints.dispatchMouseClick);
});

mapkey(',fn', '#1Open a link in non-active new tab', function () {
  Hints.create('', Hints.dispatchMouseClick, { tabbed: true, active: false });
});
mapkey(',fm', '#1Open multiple links in a new tab', function () {
  Hints.create('', Hints.dispatchMouseClick, { multipleHits: true });
});

mapkey(',fi', '#1Mouse over elements.', function () {
  Hints.create('', Hints.dispatchMouseClick, { mouseEvents: ['mouseover'] });
});
mapkey(',fo', '#1Mouse out elements.', function () {
  Hints.create('', Hints.dispatchMouseClick, { mouseEvents: ['mouseout'] });
});

map('<Ctrl-j>', ',fi');
map('<Ctrl-h>', ',fo');
map('f', ',ff');
map('F', ',fm');

// self.mappings.add('f', {
//   annotation: 'Open a link, press SHIFT to flip overlapped hints, hold SPACE to hide hints',
//   feature_group: 1,
//   repeatIgnore: true,
//   code: function() {
//     Hints.create('', Hints.dispatchMouseClick);
//   }
// });

function DefaultSetting() {
  imapkey("<Ctrl-'>", '#15Toggle quotes in an input element', toggleQuote);
  imapkey('<Ctrl-i>', '#15Open vim editor for current input', function () {
    var element = getRealEdit();
    element.blur();
    Insert.exit();
    Front.showEditor(element);
  });
  function toggleProxySite(host) {
    RUNTIME('updateProxy', {
      host: host,
      operation: 'toggle',
    });
    return true;
  }
  mapkey('cp', '#13Toggle proxy for current site', function () {
    var host = window.location.host.replace(/:\d+/, '');
    if (host && host.length) {
      toggleProxySite(host);
    }
  });
  mapkey(';cp', '#13Copy proxy info', function () {
    runtime.command(
      {
        action: 'getSettings',
        key: ['proxyMode', 'proxy', 'autoproxy_hosts'],
      },
      function (response) {
        Clipboard.write(JSON.stringify(response.settings, null, 4));
      }
    );
  });
  mapkey(';ap', '#13Apply proxy info from clipboard', function () {
    Clipboard.read(function (response) {
      var proxyConf = JSON.parse(response.data);
      runtime.command({
        action: 'updateProxy',
        operation: 'set',
        host: proxyConf.autoproxy_hosts,
        proxy: proxyConf.proxy,
        mode: proxyConf.proxyMode,
      });
    });
  });

  // create shortcuts for the command with different parameters
  map('spa', ':setProxyMode always', 0, '#13set proxy mode `always`');
  map('spb', ':setProxyMode byhost', 0, '#13set proxy mode `byhost`');
  map('spd', ':setProxyMode direct', 0, '#13set proxy mode `direct`');
  map('sps', ':setProxyMode system', 0, '#13set proxy mode `system`');
  map('spc', ':setProxyMode clear', 0, '#13set proxy mode `clear`');
  mapkey('gr', '#14Read selected text or text from clipboard', function () {
    Clipboard.read(function (response) {
      readText(window.getSelection().toString() || response.data, {
        verbose: true,
      });
    });
  });
  vmapkey('gr', '#9Read selected text', function () {
    readText(window.getSelection().toString(), { verbose: true });
  });
  mapkey('sfr', '#13show failed web requests of current page', function () {
    runtime.command(
      {
        action: 'getTabErrors',
      },
      function (response) {
        if (response.tabError && response.tabError.length) {
          var errors = response.tabError.map(function (e) {
            var url = new URL(e.url);
            return '<tr><td>{0}</td><td>{1}</td><td>{2}</td></tr>'.format(e.error, e.type, url.host);
          });
          Front.showPopup("<table style='width:100%'>{0}</table>".format(errors.join('')));
        } else {
          Front.showPopup('No errors from webRequest.');
        }
      }
    );
  });
  map('g0', ':feedkeys 99E', 0, '#3Go to the first tab');
  map('g$', ':feedkeys 99R', 0, '#3Go to the last tab');
  mapkey('zr', '#3zoom reset', function () {
    RUNTIME('setZoom', {
      zoomFactor: 0,
    });
  });
  mapkey('zi', '#3zoom in', function () {
    RUNTIME('setZoom', {
      zoomFactor: 0.1,
    });
  });
  mapkey('zo', '#3zoom out', function () {
    RUNTIME('setZoom', {
      zoomFactor: -0.1,
    });
  });

  map('ZQ', ':quit');
  mapkey('.', '#0Repeat last action', Normal.repeatLast, {
    repeatIgnore: true,
  });
  mapkey(
    'sql',
    '#0Show last action',
    function () {
      Front.showPopup(
        htmlEncode(
          runtime.conf.lastKeys
            .map(function (k) {
              return KeyboardUtils.decodeKeystroke(k);
            })
            .join(' → ')
        )
      );
    },
    { repeatIgnore: true }
  );
  mapkey('ZZ', '#5Save session and quit', function () {
    RUNTIME('createSession', {
      name: 'LAST',
      quitAfterSaved: true,
    });
  });
  mapkey('ZR', '#5Restore last session', function () {
    RUNTIME('openSession', {
      name: 'LAST',
    });
  });
  mapkey('T', '#3Choose a tab', function () {
    Front.chooseTab();
  });
  mapkey('?', '#0Show usage', function () {
    Front.showUsage();
  });
  map('u', 'e');
  mapkey('af', '#1Open a link in new tab', function () {
    Hints.create('', Hints.dispatchMouseClick, { tabbed: true });
  });
  mapkey('gf', '#1Open a link in non-active new tab', function () {
    Hints.create('', Hints.dispatchMouseClick, { tabbed: true, active: false });
  });
  mapkey('cf', '#1Open multiple links in a new tab', function () {
    Hints.create('', Hints.dispatchMouseClick, { multipleHits: true });
  });
  map('C', 'gf');
  mapkey('<Ctrl-h>', '#1Mouse over elements.', function () {
    Hints.create('', Hints.dispatchMouseClick, { mouseEvents: ['mouseover'] });
  });
  mapkey('<Ctrl-j>', '#1Mouse out elements.', function () {
    Hints.create('', Hints.dispatchMouseClick, { mouseEvents: ['mouseout'] });
  });
  mapkey('ya', '#7Copy a link URL to the clipboard', function () {
    Hints.create('*[href]', function (element) {
      Clipboard.write(element.href);
    });
  });
  mapkey('yma', '#7Copy multiple link URLs to the clipboard', function () {
    var linksToYank = [];
    Hints.create(
      '*[href]',
      function (element) {
        linksToYank.push(element.href);
        Clipboard.write(linksToYank.join('\n'));
      },
      { multipleHits: true }
    );
  });
  function getTableColumnHeads() {
    var tds = [];
    document.querySelectorAll('table').forEach(function (t) {
      var tr = t.querySelector('tr');
      if (tr) {
        tds.push(...tr.children);
      }
    });
    return tds;
  }
  mapkey('yc', '#7Copy a column of a table', function () {
    Hints.create(getTableColumnHeads(), function (element) {
      var column = Array.from(element.closest('table').querySelectorAll('tr')).map(function (tr) {
        return tr.children.length > element.cellIndex ? tr.children[element.cellIndex].innerText : '';
      });
      Clipboard.write(column.join('\n'));
    });
  });
  mapkey('ymc', '#7Copy multiple columns of a table', function () {
    var rows = null;
    Hints.create(
      getTableColumnHeads(),
      function (element) {
        var column = Array.from(element.closest('table').querySelectorAll('tr')).map(function (tr) {
          return tr.children.length > element.cellIndex ? tr.children[element.cellIndex].innerText : '';
        });
        if (!rows) {
          rows = column;
        } else {
          column.forEach(function (c, i) {
            rows[i] += '\t' + c;
          });
        }
        Clipboard.write(rows.join('\n'));
      },
      { multipleHits: true }
    );
  });
  mapkey('yq', '#7Copy pre text', function () {
    Hints.create('pre', function (element) {
      Clipboard.write(element.innerText);
    });
  });
  mapkey('i', '#1Go to edit box', function () {
    Hints.create('input, textarea, *[contenteditable=true], select', Hints.dispatchMouseClick);
  });
  mapkey('gi', '#1Go to the first edit box', function () {
    Hints.createInputLayer();
  });
  mapkey('I', '#1Go to edit box with vim editor', function () {
    Hints.create('input, textarea, *[contenteditable=true], select', function (element) {
      Front.showEditor(element);
    });
  });
  mapkey('O', '#1Open detected links from text', function () {
    Hints.create(
      runtime.conf.clickablePat,
      function (element) {
        window.location.assign(element[2]);
      },
      { statusLine: 'Open detected links from text' }
    );
  });

  mapkey(';q', 'Toggle mouseSelectToQuery', function () {
    runtime.command({
      action: 'toggleMouseQuery',
      origin: window.location.origin,
    });
  });

  mapkey(';s', 'Toggle PDF viewer from SurfingKeys', function () {
    var pdfUrl = window.location.href;
    if (pdfUrl.indexOf(chrome.extension.getURL('/pages/pdf_viewer.html')) === 0) {
      pdfUrl = window.location.search.substr(3);
      chrome.storage.local.set({ noPdfViewer: 1 }, function () {
        window.location.replace(pdfUrl);
      });
    } else {
      if (document.querySelector('EMBED') && document.querySelector('EMBED').getAttribute('type') === 'application/pdf') {
        chrome.storage.local.remove('noPdfViewer', function () {
          window.location.replace(pdfUrl);
        });
      } else {
        chrome.storage.local.get('noPdfViewer', function (resp) {
          if (!resp.noPdfViewer) {
            chrome.storage.local.set({ noPdfViewer: 1 }, function () {
              Front.showBanner('PDF viewer disabled.');
            });
          } else {
            chrome.storage.local.remove('noPdfViewer', function () {
              Front.showBanner('PDF viewer enabled.');
            });
          }
        });
      }
    }
  });
  map('<Ctrl-i>', 'I');
  cmap('<ArrowDown>', '<Tab>');
  cmap('<ArrowUp>', '<Shift-Tab>');
  cmap('<Ctrl-n>', '<Tab>');
  cmap('<Ctrl-p>', '<Shift-Tab>');
  mapkey('q', '#1Click on an Image or a button', function () {
    Hints.create('img, button', Hints.dispatchMouseClick);
  });
  mapkey('<Alt-i>', '#0enter PassThrough mode to temporarily suppress SurfingKeys', function () {
    Normal.passThrough();
  });
  mapkey('<Alt-p>', '#3pin/unpin current tab', function () {
    RUNTIME('togglePinTab');
  });
  mapkey('<Alt-m>', '#3mute/unmute current tab', function () {
    RUNTIME('muteTab');
  });
  mapkey(
    'B',
    '#4Go one tab history back',
    function () {
      RUNTIME('historyTab', { backward: true });
    },
    { repeatIgnore: true }
  );
  mapkey(
    'F',
    '#4Go one tab history forward',
    function () {
      RUNTIME('historyTab', { backward: false });
    },
    { repeatIgnore: true }
  );
  mapkey('<Ctrl-6>', '#4Go to last used tab', function () {
    RUNTIME('goToLastTab');
  });
  mapkey(
    'gT',
    '#4Go to first activated tab',
    function () {
      RUNTIME('historyTab', { index: 0 });
    },
    { repeatIgnore: true }
  );
  mapkey(
    'gt',
    '#4Go to last activated tab',
    function () {
      RUNTIME('historyTab', { index: -1 });
    },
    { repeatIgnore: true }
  );
  mapkey(
    'S',
    '#4Go back in history',
    function () {
      history.go(-1);
    },
    { repeatIgnore: true }
  );
  mapkey(
    'D',
    '#4Go forward in history',
    function () {
      history.go(1);
    },
    { repeatIgnore: true }
  );
  mapkey('r', '#4Reload the page', function () {
    RUNTIME('reloadTab', { nocache: false });
  });

  mapkey('go', '#8Open a URL in current tab', function () {
    Front.openOmnibar({ type: 'URLs', extra: 'getAllSites', tabbed: false });
  });
  mapkey('oi', '#8Open incognito window', function () {
    runtime.command({
      action: 'openIncognito',
      url: window.location.href,
    });
  });
  mapkey('ox', '#8Open recently closed URL', function () {
    Front.openOmnibar({ type: 'URLs', extra: 'getRecentlyClosed' });
  });
  mapkey('H', '#8Open opened URL in current tab', function () {
    Front.openOmnibar({ type: 'URLs', extra: 'getTabURLs' });
  });
  mapkey('Q', '#8Open omnibar for word translation', function () {
    Front.openOmniquery({
      query: Visual.getWordUnderCursor(),
      style: 'opacity: 0.8;',
    });
  });
  mapkey('b', '#8Open a bookmark', function () {
    Front.openOmnibar({ type: 'Bookmarks' });
  });
  mapkey('ab', '#8Bookmark current page to selected folder', function () {
    var page = {
      url: window.location.href,
      title: document.title,
    };
    Front.openOmnibar({ type: 'AddBookmark', extra: page });
  });
  mapkey('oh', '#8Open URL from history', function () {
    Front.openOmnibar({ type: 'History' });
  });
  mapkey('om', '#8Open URL from vim-like marks', function () {
    Front.openOmnibar({ type: 'VIMarks' });
  });
  mapkey(':', '#8Open commands', function () {
    Front.openOmnibar({ type: 'Commands' });
  });
  mapkey('zv', '#9Enter visual mode, and select whole element', function () {
    Visual.toggle('z');
  });
  mapkey('yv', '#7Yank text of an element', function () {
    Visual.toggle('y');
  });
  mapkey('ymv', '#7Yank text of multiple elements', function () {
    Visual.toggle('ym');
  });
  mapkey('yi', '#7Yank text of an input', function () {
    Hints.create('input, textarea, select', function (element) {
      Clipboard.write(element.value);
    });
  });
  mapkey('V', '#9Restore visual mode', function () {
    Visual.restore();
  });
  mapkey('*', '#9Find selected text in current page', function () {
    Visual.star();
    Visual.toggle();
  });
  vmapkey('<Ctrl-u>', '#9Backward 20 lines', function () {
    Visual.feedkeys('20k');
  });
  vmapkey('<Ctrl-d>', '#9Forward 20 lines', function () {
    Visual.feedkeys('20j');
  });
  mapkey('x', '#3Close current tab', function () {
    RUNTIME('closeTab');
  });
  mapkey('X', '#3Restore closed tab', function () {
    RUNTIME('openLast');
  });
  mapkey('W', '#3New window with current tab', function () {
    RUNTIME('newWindow');
  });
  mapkey('m', '#10Add current URL to vim-like marks', Normal.addVIMark);
  mapkey("'", '#10Jump to vim-like mark', Normal.jumpVIMark);
  mapkey("<Ctrl-'>", '#10Jump to vim-like mark in new tab.', function (mark) {
    Normal.jumpVIMark(mark);
  });
  mapkey('<<', '#3Move current tab to left', function () {
    RUNTIME('moveTab', {
      step: -1,
    });
  });
  mapkey('>>', '#3Move current tab to right', function () {
    RUNTIME('moveTab', {
      step: 1,
    });
  });
  mapkey('w', '#2Switch frames', function () {
    Normal.rotateFrame();
  });
  mapkey(';w', '#2Focus top window', function () {
    top.focus();
  });
  mapkey('cc', '#7Open selected link or link from clipboard', function () {
    if (window.getSelection().toString()) {
      tabOpenLink(window.getSelection().toString());
    } else {
      Clipboard.read(function (response) {
        tabOpenLink(response.data);
      });
    }
  });
  mapkey('[[', '#1Click on the previous link on current page', previousPage);
  mapkey(']]', '#1Click on the next link on current page', nextPage);
  mapkey('ys', "#7Copy current page's source", function () {
    var aa = document.documentElement.cloneNode(true);
    Clipboard.write(aa.outerHTML);
  });
  mapkey('yj', '#7Copy current settings', function () {
    runtime.command(
      {
        action: 'getSettings',
        key: 'RAW',
      },
      function (response) {
        Clipboard.write(JSON.stringify(response.settings, null, 4));
      }
    );
  });
  mapkey(';pj', '#7Restore settings data from clipboard', function () {
    Clipboard.read(function (response) {
      RUNTIME('updateSettings', {
        settings: JSON.parse(response.data.trim()),
      });
    });
  });
  mapkey('yd', '#7Copy current downloading URL', function () {
    runtime.command(
      {
        action: 'getDownloads',
        query: { state: 'in_progress' },
      },
      function (response) {
        var items = response.downloads.map(function (o) {
          return o.url;
        });
        Clipboard.write(items.join(','));
      }
    );
  });
  mapkey('yt', '#3Duplicate current tab', function () {
    RUNTIME('duplicateTab');
  });
  mapkey('yy', "#7Copy current page's URL", function () {
    Clipboard.write(window.location.href);
  });
  mapkey('yh', "#7Copy current page's host", function () {
    var url = new URL(window.location.href);
    Clipboard.write(url.host);
  });
  mapkey('yl', "#7Copy current page's title", function () {
    Clipboard.write(document.title);
  });
  mapkey('yQ', '#7Copy all query history of OmniQuery.', function () {
    runtime.command(
      {
        action: 'getSettings',
        key: 'OmniQueryHistory',
      },
      function (response) {
        Clipboard.write(response.settings.OmniQueryHistory.join('\n'));
      }
    );
  });
  function generateFormKey(form) {
    return (form.method || 'get') + '::' + new URL(form.action).pathname;
  }
  mapkey('yf', '#7Copy form data in JSON on current page', function () {
    var fd = {};
    document.querySelectorAll('form').forEach(function (form) {
      fd[generateFormKey(form)] = getFormData(form, 'json');
    });
    Clipboard.write(JSON.stringify(fd, null, 4));
  });
  mapkey(';pf', '#7Fill form with data from yf', function () {
    Hints.create('form', function (element, event) {
      var formKey = generateFormKey(element);
      Clipboard.read(function (response) {
        var forms = JSON.parse(response.data.trim());
        if (forms.hasOwnProperty(formKey)) {
          var fd = forms[formKey];
          element.querySelectorAll('input').forEach(function (ip) {
            if (fd.hasOwnProperty(ip.name) && typeof fd[ip.name] !== 'object') {
              ip.value = fd[ip.name];
            }
          });
        } else {
          Front.showBanner('No form data found for your selection from clipboard.');
        }
      });
    });
  });
  mapkey('yg', '#7Capture current page', function () {
    Front.toggleStatus(false);
    setTimeout(function () {
      runtime.command(
        {
          action: 'captureVisibleTab',
        },
        function (response) {
          Front.toggleStatus(true);
          Front.showPopup("<img src='{0}' />".format(response.dataUrl));
        }
      );
    }, 500);
  });
  mapkey('yp', '#7Copy form data for POST on current page', function () {
    var aa = [];
    document.querySelectorAll('form').forEach(function (form) {
      var fd = {};
      fd[(form.method || 'get') + '::' + form.action] = getFormData(form);
      aa.push(fd);
    });
    Clipboard.write(JSON.stringify(aa, null, 4));
  });
  mapkey('ob', '#8Open Search with alias b', function () {
    Front.openOmnibar({ type: 'SearchEngine', extra: 'b' });
  });
  mapkey('og', '#8Open Search with alias g', function () {
    Front.openOmnibar({ type: 'SearchEngine', extra: 'g' });
  });
  mapkey('od', '#8Open Search with alias d', function () {
    Front.openOmnibar({ type: 'SearchEngine', extra: 'd' });
  });
  mapkey('ow', '#8Open Search with alias w', function () {
    Front.openOmnibar({ type: 'SearchEngine', extra: 'w' });
  });
  mapkey('oy', '#8Open Search with alias y', function () {
    Front.openOmnibar({ type: 'SearchEngine', extra: 'y' });
  });
  if (window.navigator.userAgent.indexOf('Firefox') > 0) {
    mapkey('on', '#3Open newtab', function () {
      tabOpenLink('about:blank');
    });
  } else {
    mapkey('on', '#3Open newtab', function () {
      tabOpenLink('chrome://newtab/');
    });
    mapkey('ga', '#12Open Chrome About', function () {
      tabOpenLink('chrome://help/');
    });
    mapkey('gb', '#12Open Chrome Bookmarks', function () {
      tabOpenLink('chrome://bookmarks/');
    });
    mapkey('gc', '#12Open Chrome Cache', function () {
      tabOpenLink('chrome://cache/');
    });
    mapkey('gd', '#12Open Chrome Downloads', function () {
      tabOpenLink('chrome://downloads/');
    });
    mapkey('gh', '#12Open Chrome History', function () {
      tabOpenLink('chrome://history/');
    });
    mapkey('gk', '#12Open Chrome Cookies', function () {
      tabOpenLink('chrome://settings/content/cookies');
    });
    mapkey('ge', '#12Open Chrome Extensions', function () {
      tabOpenLink('chrome://extensions/');
    });
    mapkey('gn', '#12Open Chrome net-internals', function () {
      tabOpenLink('chrome://net-internals/#proxy');
    });
    mapkey('si', '#12Open Chrome Inspect', function () {
      tabOpenLink('chrome://inspect/#devices');
    });
    mapkey('<Ctrl-Alt-d>', '#11Mermaid diagram generator', function () {
      tabOpenLink('/pages/mermaid.html');
    });
  }
  mapkey('gs', '#12View page source', function () {
    RUNTIME('viewSource', { tab: { tabbed: true } });
  });
  mapkey('gu', '#4Go up one path in the URL', function () {
    var pathname = location.pathname;
    if (pathname.length > 1) {
      pathname = pathname.endsWith('/') ? pathname.substr(0, pathname.length - 1) : pathname;
      var last = pathname.lastIndexOf('/'),
        repeats = RUNTIME.repeats;
      RUNTIME.repeats = 1;
      while (repeats-- > 1) {
        var p = pathname.lastIndexOf('/', last - 1);
        if (p === -1) {
          break;
        } else {
          last = p;
        }
      }
      pathname = pathname.substr(0, last);
    }
    window.location.href = location.origin + pathname;
  });
  mapkey('g?', '#4Reload current page without query string(all parts after question mark)', function () {
    window.location.href = window.location.href.replace(/\?[^\?]*$/, '');
  });
  mapkey('g#', '#4Reload current page without hash fragment', function () {
    window.location.href = window.location.href.replace(/\#[^\#]*$/, '');
  });
  mapkey('gU', '#4Go to root of current URL hierarchy', function () {
    window.location.href = window.location.origin;
  });
  mapkey('gxt', '#3Close tab on left', function () {
    RUNTIME('closeTabLeft');
  });
  mapkey('gxT', '#3Close tab on right', function () {
    RUNTIME('closeTabRight');
  });
  mapkey('gx0', '#3Close all tabs on left', function () {
    RUNTIME('closeTabsToLeft');
  });
  mapkey('gx$', '#3Close all tabs on right', function () {
    RUNTIME('closeTabsToRight');
  });
  mapkey('gxx', '#3Close all tabs except current one', function () {
    RUNTIME('tabOnly');
  });
  mapkey('se', '#11Edit Settings', function () {
    tabOpenLink('/pages/options.html');
  });
  mapkey('sm', '#11Preview markdown', function () {
    tabOpenLink('/pages/markdown.html');
  });
  mapkey('su', '#4Edit current URL with vim editor, and open in new tab', function () {
    Front.showEditor(
      window.location.href,
      function (data) {
        tabOpenLink(data);
      },
      'url'
    );
  });
  mapkey('sU', '#4Edit current URL with vim editor, and reload', function () {
    Front.showEditor(
      window.location.href,
      function (data) {
        window.location.href = data;
      },
      'url'
    );
  });
  mapkey(';m', '#1mouse out last element', function () {
    Hints.mouseoutLastElement();
  });
  mapkey(';j', '#12Close Downloads Shelf', function () {
    RUNTIME('closeDownloadsShelf', { clearHistory: true });
  });
  mapkey(';pp', '#7Paste html on current page', function () {
    Clipboard.read(function (response) {
      document.documentElement.removeAttributes();
      document.body.removeAttributes();
      setInnerHTML(document.head, '<title>' + new Date() + ' updated by Surfingkeys</title>');
      setInnerHTML(document.body, response.data);
    });
  });
  mapkey(';i', '#14Insert jquery library on current page', function () {
    Normal.insertJS('//ajax.aspnetcdn.com/ajax/jQuery/jquery-2.1.4.min.js');
  });
  mapkey(';t', 'Translate selected text with google', function () {
    searchSelectedWith('https://translate.google.com/?hl=en#auto/en/', false, false, '');
  });
  mapkey(';dh', '#14Delete history older than 30 days', function () {
    RUNTIME('deleteHistoryOlderThan', {
      days: 30,
    });
  });
  mapkey(';db', '#14Remove bookmark for current page', function () {
    RUNTIME('removeBookmark');
  });

  addSearchAliasX(
    'g',
    'google',
    'https://www.google.com/search?q=',
    's',
    'https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=',
    function (response) {
      var res = JSON.parse(response.text);
      return res[1];
    }
  );
  addSearchAliasX('d', 'duckduckgo', 'https://duckduckgo.com/?q=', 's', 'https://duckduckgo.com/ac/?q=', function (response) {
    var res = JSON.parse(response.text);
    return res.map(function (r) {
      return r.phrase;
    });
  });
  addSearchAliasX(
    'w',
    'bing',
    'http://global.bing.com/search?setmkt=en-us&setlang=en-us&q=',
    's',
    'http://api.bing.com/osjson.aspx?query=',
    function (response) {
      var res = JSON.parse(response.text);
      return res[1];
    }
  );
  addSearchAliasX('s', 'stackoverflow', 'http://stackoverflow.com/search?q=');
  addSearchAliasX('h', 'github', 'https://github.com/search?type=Code&utf8=%E2%9C%93&q=');
  addSearchAliasX(
    'y',
    'youtube',
    'https://www.youtube.com/results?search_query=',
    's',
    'https://clients1.google.com/complete/search?client=youtube&ds=yt&callback=cb&q=',
    function (response) {
      var res = JSON.parse(response.text.substr(9, response.text.length - 10));
      return res[1].map(function (d) {
        return d[0];
      });
    }
  );

  document.dispatchEvent(new CustomEvent('surfingkeys:defaultSettingsLoaded'));
}

// Section: Example folding syntax {{{1
//  --------------------------------------------------------------------------
//  --------------------------------------------------------------------------
// }}}

console.log('Load all surfingKeys config'); // REMOVETAG
