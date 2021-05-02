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
      var allResult = document.getElementById('results-contents');
      if (allResult) {
        console.log(allResult);
        var element = document.getElementById('webTrans');
        element.parentNode.removeChild(element);

        return allResult.innerHTML;
      } else if (collinsResult) {
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

youdao();

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
settings.autoSpeakOnInlineQuery = true;

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


function stringClean(str) {
  // return str.replace(/\n/g, '').replace(/ /g, '');
  return str.replace(/\n/g, '');
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

// Section: Domain/Site specific command {{{1
//  --------------------------------------------------------------------------

// Section: Atlassian JIRA {{{2
//  --------------------------------------------------------------------------
const atlassianJIRAMapping = () => {
  let domainMap = {
      domain: /(jira-.*)/i,
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
            cd = '🔴' + ' [' + 'B:' + cd + ']';
            break;
          default:
            cd = '[' + 'I:' + cd + ']';
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

//  --------------------------------------------------------------------------
// }}}
//  --------------------------------------------------------------------------
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

mapkey(
  ',y',
  'Copy URL TO MarkDown',
  function () {

    //content = document.getElementById('title-text') ? document.getElementById('title-text').textContent "Unknown"
    let title = stringClean(document.title),
      url = window.location.href


    md = "[" + title + "]" + '(' + url + ')' + ' '

    console.log(md);
    Clipboard.write(md);
  },
  {}
);

// Section: Example folding syntax {{{1
//  --------------------------------------------------------------------------
//  --------------------------------------------------------------------------
// }}}



atlassianJIRAMapping();
gitlabMapping();

githubMapping();
devMapping();
googleMapping();

console.log('Load all simple surfingKeys config'); // REMOVETAG