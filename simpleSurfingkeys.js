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

// https://gist.github.com/idelem/a2b15c4fe7613487e16fb55ba3af1be9
function copyToClipboard(text) {
  Front.showBanner('Coping: ' + text);
  if (window.clipboardData && window.clipboardData.setData) {
    console.info('window.clipboardData');
    /*IE specific code path to prevent textarea being shown while dialog is visible.*/
    return clipboardData.setData('Text', text);
  } else if (document.queryCommandSupported && document.queryCommandSupported('copy')) {
    console.info('Create a child object to copy');
    var textarea = document.createElement('textarea');
    textarea.textContent = text;
    textarea.style.position = 'fixed'; /* Prevent scrolling to bottom of page in MS Edge.*/
    document.body.appendChild(textarea);
    textarea.select();
    try {
      // document.execCommand('copy'); /* Security exception may be thrown by some browsers.*/
      // Front.showBanner('Copied: ' + text);
    } catch (ex) {
      console.warn('Copy to clipboard failed.', ex);
      return false;
    } finally {
      setTimeout(() =>{
        document.body.removeChild(textarea);
      }, 7000);
    }

    setTimeout(() =>{
      Front.showBanner("Select");
      textarea.select();
    }, 1000);
    setTimeout(() =>{
      Front.showBanner("copy");
      document.execCommand('copy')
    }, 2000);
  }
}

function stringClean(str) {
  // return str.replace(/\n/g, '').replace(/ /g, '');
  return str.replace(/\n/g, '');
}

// elementClick
let eClick = (selectors) => () => document.querySelectorAll(selectors).forEach((e) => e.click());
let tClick =
  (s, tag = 'a') =>
  () =>
    getElementsByText(s, tag)[0].click();

// }}}
// Section: Domain/Site specific mapping {{{1
//  --------------------------------------------------------------------------

// Section: Atlassian JIRA {{{2
//  --------------------------------------------------------------------------
const atlassianJIRAMapping = () => {
  let domainMap = { domain: /(jira-.*)/i },
    jstate = { sideHidden: true };
  // Section: Fns {{{3
  //  --------------------------------------------------------------------------
  function jiraLinkCopy() {
    // let header = document.querySelectorAll('h1[data-test-id="issue.views.issue-base.foundation.summary.heading"]')[0].textContent,
    let header = document.querySelectorAll('.editable-field')[0].textContent;
    (url = window.location.href), (cd = url.match(/(CD|IDUN)-[0-9]*/)[0]);

    let type = document.querySelectorAll('#issuedetails .item #type-val')[0],
      typeText = '';

    if (type) {
      typeText = type.textContent.replace(/\n/g, '').replace(/ /g, '');
      switch (typeText) {
        case 'Requirement':
          cd = '🟠' + '  [' + 'R:' + cd + ']';
          break;
        case 'Epic':
          cd = '🟣' + '  [' + 'E:' + cd + ']';
          break;
        case 'Story':
          cd = '🟢' + '  [' + 'S:' + cd + ']';
          break;
        case 'Bug':
          cd = '🔴' + '  [' + 'B:' + cd + ']';
          break;
        case 'Task':
          cd = '🔵' + '  [' + 'T:' + cd + ']';
          break;
        default:
          cd = '[' + 'I:' + cd + ']';
      }

      typeText = typeText + ': ';
    }

    md = cd + '(' + url + ')' + ' ' + typeText + header;

    console.log(typeText);
    console.log(md);
    copyToClipboard(md);
  }
  function cleanView() {
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
  }
  // }}}3
  mapkey(',zm', 'Toggle navigation bar', () => document.querySelectorAll('button[aria-label="Toggle navigation"]')[0].click(), domainMap);
  mapkey(',,y', 'Copy JIRA TO MarkDown', jiraLinkCopy, domainMap);
  mapkey('`c', 'Copy JIRA TO MarkDown', jiraLinkCopy, domainMap);
  mapkey(',,c', 'Clean view', cleanView, domainMap);
  mapkey(',pr', 'Open pull request', () => getElementsByText('pull request', (tag = 'span'))[0].click(), domainMap);
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
      copyToClipboard('git clone ' + window.location.href + '.git');
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

const state = { scheduleHidden: true };

const gitlabMapping = () => {
  const gitlabDomain = { domain: /gitlab/i };

  // Fns {{{3
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

    console.log('Hide detached');
    let pipeline = document.querySelectorAll('.ci-table .commit');
    pipeline.forEach((r) => {
      // if (!String(r.children[2].children[0].href).includes('yongqinchuan')) {
      //   r.hidden = state.scheduleHidden;
      // }

      if (r.children[1].innerText.match('detached') != null) {
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
    copyToClipboard(md);
  };

  const createTLink = () => {
    console.log('github convert link');
    // let c = document.querySelectorAll('.pipeline-schedule-table-row')[0].children[2]

    // Update schedule page
    let rows = document.querySelectorAll('.pipeline-schedule-table-row');
    rows.forEach((r) => {
      let pipe = r.children[2];
      let branch = r.children[1].innerText.replace(' ', '');
      let url = branch == 'develop' ? 'http://pipeline/robot/develop/' : 'http://pipeline/robot/mr/';

      if (pipe.children.length == 1) {
        let newC = pipe.children[0].cloneNode(true);
        let number = newC.innerText.replace('\n', '').replace(' ', '').replace('#', '');

        newC.children[0].href = url + number + '/report.html';
        newC.children[0].target = '_blank';
        newC.children[0].children[1].textContent = 'Report';

        pipe.appendChild(newC);

        // Cleanup pipe number
        let C = pipe.children[0];
        C.children[0].children[1].textContent = number;
      } else if (pipe.children.length == 2) {
        pipe.removeChild(pipe.children[1]);
      }
    });

    // Update header link
    let ciButtons = document.querySelectorAll('.ci-status');
    ciButtons.forEach((ciStatusButton) => {
      let url = 'http://pipeline/robot/mr/',
        knownPathRegex = /(pipelines\/\d*)/,
        number = '',
        PUrl = ciStatusButton.href,
        knownPath = PUrl.match(knownPathRegex);

      if (knownPath != null) {
        number = knownPath[0].replace(/pipelines\//, '');

        newButton = ciStatusButton.cloneNode(true);
        newButton.textContent = 'Report';
        newButton.target = '_blank';
        newButton.href = url + number + '/report.html';

        ciStatusButton.parentElement.appendChild(newButton);
      }
    });
  };
  // }}}3
  mapkey(',,l', 'Gitlab: Create Test link', createTLink, gitlabDomain);
  mapkey(',,y', 'Gitlab: copy mr/pipeline link in MD', gitlabCopyUrl, gitlabDomain);
  mapkey(',,c', 'Gitlab: github clean', cleanSchedule, gitlabDomain);
  mapkey(',,u', 'Gitlab: go one level up', gitlabOneLevelUp, gitlabDomain);
  mapkey('\\s', 'Gitlab: toggle sidebar', gitlabToggleSidebar, gitlabDomain);
  mapkey(',,s', 'Gitlab: go to schedule', tClick('Schedules'), gitlabDomain);
  mapkey(',,m', 'Gitlab: go to mr', eClick('.shortcuts-merge_requests'), gitlabDomain);
  mapkey(',,p', 'Gitlab: go to pipeline', eClick('.shortcuts-pipelines'), gitlabDomain);
  mapkey(',,f', 'Gitlab: go to Files', tClick('Files'), gitlabDomain);
  mapkey(',,g', 'Gitlab: go to Find file', tClick('Find file'), gitlabDomain);
  mapkey(',,w', 'Gitlab: Wiki', tClick('Wiki'), gitlabDomain);
  mapkey(',,zm', 'Gitlab: fold all diff', eClick('.file-title'), gitlabDomain);

  // New shortcut for left hand only
  mapkey('``g', 'Gitlab: Create Test link', createTLink, gitlabDomain);
  mapkey('``c', 'Gitlab: copy mr/pipeline link in MD', gitlabCopyUrl, gitlabDomain);
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

  mapkey(',,r', 'Related', () => document.querySelectorAll('.network-list .network-view-card')[0].click(), devDomain);
  mapkey(',,f', 'Select first', () => document.querySelectorAll('.network-view-card')[0].click(), devDomain);
  mapkey(',,c', 'Clean all notification', () => document.querySelectorAll('.notifications-tc .notification').forEach((e) => e.click()), devDomain);
  mapkey(',,b', 'Net Back', () => document.querySelectorAll('.network-back-btn').forEach((e) => e.click()), devDomain);
  mapkey(
    ',,d',
    'debug',
    () => {
      debugger;
    },
    {}
  );
};

// }}}1
// Section: Define default Mapping {{{1
//  --------------------------------------------------------------------------

mapkey('gE', '#12 go Extensions - Open Chrome extensions Shortcut setting', () => tabOpenLink('chrome://extensions/shortcuts'));

// # Insert your preferred key mappings here.
// map S scrollFullPageUp
mapkey('gp', '#3pin/unpin current tab', () => RUNTIME('togglePinTab'));
map('<Ctrl-i>', '<Alt-s>');
map('<Ctrl-z>', '<Alt-s>');
map('H', 'S');
map('J', '<<');
map('K', '>>');
map('L', 'D');
map('h', 'E');
map('l', 'R');
map('s', 'u');

// Rewrite default Keybindings
mapkey('?', '#0Show usage', () => Front.showUsage());
mapkey('t', '#8Open a URL', () => Front.openOmnibar({ type: 'URLs', extra: 'getAllSites' }));
mapkey(':', '#8Open commands', () => Front.openOmnibar({ type: 'Commands' }));

function copyTextToMarkdown() {
  // {{{2
  var markdown = '[' + document.title + '](' + window.location.href + ')';
  var selection = window.getSelection().toString();
  if (selection.length != 0) {
    selection = '\n' + selection;
  }
  copyToClipboard(markdown + selection);
  console.log(markdown);
}
//}}}2

mapkey(',y', 'Copy URL TO MarkDown ALL', copyTextToMarkdown);
mapkey('`c', 'Copy URL TO MarkDown', copyTextToMarkdown);

atlassianJIRAMapping();
gitlabMapping();
githubMapping();
devMapping();
googleMapping();

console.log('Load all simple surfingKeys config');

// }}}1

// Section: Styles {{{1
//  --------------------------------------------------------------------------

// Styles from : https://gist.github.com/emraher/2c071182ce0f04f3c69f6680de335029#file-surfingkeysdraculathemeattempt-txt-l1
// -----------------------------------------------------------------------------------------------------------------------
// Change hints styles
// -----------------------------------------------------------------------------------------------------------------------
// Hints.style('border: solid 1px #ff79c6; color:#44475a; background: #f1fa8c; background-color: #f1fa8c; font-size: 10pt; font-family: "Fira Code"');
// Mine
Hints.style('color: #e2e3e7; font-size: 10px; text-shadow: none; background: #383c4a; border: 0.25em solid #383c4a; border-radius: 0.68em; box-shadow: 0em 0.1em 0.6em 0.1em rgba(0, 0, 0, 0.4); ');
Hints.style('border: solid 2px #ff79c6;padding: 1px; color: #e2e3e7; font-size: 10px; text-shadow: none; background: #808596; border: 0.25em solid #383c4a; border-radius: 0.68em; box-shadow: 0em 0.1em 0.6em 0.1em rgba(0, 0, 0, 0.4); ', 'text');

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
// Section: Example folding syntax {{{1
//  --------------------------------------------------------------------------
//  --------------------------------------------------------------------------
// }}}
