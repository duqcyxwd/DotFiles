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
//    Chuan's 2023 surfing key setting

// Section: Basic setting {{{1
//  --------------------------------------------------------------------------
api.Hints.characters = 'asdfgqwertvbn';
api.Hints.numericHints = false;
api.Hints.filterHints = false;

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

// elementClick
let eClick = (selectors) => () => document.querySelectorAll(selectors).forEach((e) => e.click());
let tClick = (s, tag = 'a') => () => getElementsByText(s, tag)[0].click();

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
    api.Clipboard.write(md);
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
  api.mapkey(',zm', 'Toggle navigation bar', () => document.querySelectorAll('button[aria-label="Toggle navigation"]')[0].click(), domainMap);
  api.mapkey(',,y', 'Copy JIRA TO MarkDown', jiraLinkCopy, domainMap);
  api.mapkey('`c', 'Copy JIRA TO MarkDown', jiraLinkCopy, domainMap);
  api.mapkey(',,c', 'Clean view', cleanView, domainMap);
  api.mapkey(',pr', 'Open pull request', () => getElementsByText('pull request', (tag = 'span'))[0].click(), domainMap);
};

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
    api.Clipboard.write(md);
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
  api.mapkey(',,l', 'Gitlab: Create Test link', createTLink, gitlabDomain);
  api.mapkey(',,y', 'Gitlab: copy mr/pipeline link in MD', gitlabCopyUrl, gitlabDomain);
  api.mapkey(',,c', 'Gitlab: github clean', cleanSchedule, gitlabDomain);
  api.mapkey(',,u', 'Gitlab: go one level up', gitlabOneLevelUp, gitlabDomain);
  api.mapkey('\\s', 'Gitlab: toggle sidebar', gitlabToggleSidebar, gitlabDomain);
  api.mapkey(',,s', 'Gitlab: go to schedule', tClick('Schedules'), gitlabDomain);
  api.mapkey(',,m', 'Gitlab: go to mr', eClick('.shortcuts-merge_requests'), gitlabDomain);
  api.mapkey(',,p', 'Gitlab: go to pipeline', eClick('.shortcuts-pipelines'), gitlabDomain);
  api.mapkey(',,f', 'Gitlab: go to Files', tClick('Files'), gitlabDomain);
  api.mapkey(',,g', 'Gitlab: go to Find file', tClick('Find file'), gitlabDomain);
  api.mapkey(',,w', 'Gitlab: Wiki', tClick('Wiki'), gitlabDomain);
  api.mapkey(',,zm', 'Gitlab: fold all diff', eClick('.file-title'), gitlabDomain);

  // New shortcut for left hand only
  api.mapkey('``g', 'Gitlab: Create Test link', createTLink, gitlabDomain);
  api.mapkey('``c', 'Gitlab: copy mr/pipeline link in MD', gitlabCopyUrl, gitlabDomain);
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

  api.mapkey(',,c', 'Google: Ad clean', cleanAd, googleDomain);
  api.mapkey(',,,', 'Google: Ad clean', cleanAd, googleDomain);
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

  api.mapkey(',,r', 'Related', () => document.querySelectorAll('.network-list .network-view-card')[0].click(), devDomain);
  api.mapkey(',,f', 'Select first', () => document.querySelectorAll('.network-view-card')[0].click(), devDomain);
  api.mapkey(',,c', 'Clean all notification', () => document.querySelectorAll('.notifications-tc .notification').forEach((e) => e.click()), devDomain);
  api.mapkey(',,b', 'Net Back', () => document.querySelectorAll('.network-back-btn').forEach((e) => e.click()), devDomain);
  api.mapkey(
    ',,d',
    'debug',
    () => {
      debugger;
    },
    {}
  );
};

// }}}1

// Section: My Custom Mapping {{{1
//  --------------------------------------------------------------------------
var cleanAlias = function () {
  removeSearchAliasX('g');
  removeSearchAliasX('d');
  removeSearchAliasX('b');
  removeSearchAliasX('w');
  removeSearchAliasX('s');
  removeSearchAliasX('h');
  removeSearchAliasX('y');
};
// cleanAlias();

// mapkey('gE', '#12 go Extensions - Open Chrome extensions Shortcut setting', () => tabOpenLink('chrome://extensions/shortcuts'));

// # Insert your preferred key mappings here.
// map S scrollFullPageUp
// mapkey('gp', '#3pin/unpin current tab', () => RUNTIME('togglePinTab'));
// map('<Ctrl-i>', '<Alt-s>');
api.map('<Ctrl-z>', '<Alt-s>');
api.map('H', 'S');
api.map('J', '<<');
api.map('K', '>>');
api.map('L', 'D');
api.map('h', 'E');
api.map('l', 'R');
api.map('s', 'u');

// Rewrite default Keybindings
// mapkey('?', '#0Show usage', () => Front.showUsage());
// mapkey('t', '#8Open a URL', () => Front.openOmnibar({ type: 'URLs', extra: 'getAllSites' }));
// mapkey(':', '#8Open commands', () => Front.openOmnibar({ type: 'Commands' }));

function copyTextToMarkdown() {
  // {{{2
  var markdown = '[' + document.title + '](' + window.location.href + ')';
  var selection = window.getSelection().toString();
  if (selection.length != 0) {
    selection = '\n' + selection;
  }
  api.Clipboard.write(markdown + selection);
  console.log(api)
  console.log(markdown);
}
//}}}2

api.mapkey(',y', 'Copy URL TO MarkDown', copyTextToMarkdown);
api.mapkey('`c', 'Copy URL TO MarkDown', copyTextToMarkdown);

atlassianJIRAMapping();
gitlabMapping();
devMapping();
googleMapping();

console.log('Load all simple surfingKeys config');

// }}}1
// Section: Example folding syntax {{{1
//  --------------------------------------------------------------------------
//  --------------------------------------------------------------------------
// }}}

// Section: Theme {{{1
//  --------------------------------------------------------------------------
// Section: Theme Rosé Pine {{{2
//  --------------------------------------------------------------------------
// name: Rosé Pine
// author: thuanowa
// license: unlicense
// upstream: https://github.com/rose-pine/surfingkeys/blob/main/dist/rose-pine.conf
// blurb: All natural pine, faux fur and a bit of soho vibes for the classy minimalist

const hintsCss =
  "font-size: 13pt; font-family: 'JetBrains Mono NL', 'Cascadia Code', 'Helvetica Neue', Helvetica, Arial, sans-serif; border: 0px; color: #e0def4 !important; background: #191724; background-color: #191724";

api.Hints.style(hintsCss);
api.Hints.style(hintsCss, "text");

settings.theme = `
  .sk_theme {
    background: #191724;
    color: #e0def4;
  }
  .sk_theme input {
    color: #e0def4;
  }
  .sk_theme .url {
    color: #c4a7e7;
  }
  .sk_theme .annotation {
    color: #ebbcba;
  }
  .sk_theme kbd {
    background: #26233a;
    color: #e0def4;
  }
  .sk_theme .frame {
    background: #1f1d2e;
  }
  .sk_theme .omnibar_highlight {
    color: #403d52;
  }
  .sk_theme .omnibar_folder {
    color: #e0def4;
  }
  .sk_theme .omnibar_timestamp {
    color: #9ccfd8;
  }
  .sk_theme .omnibar_visitcount {
    color: #9ccfd8;
  }
  .sk_theme .prompt, .sk_theme .resultPage {
    color: #e0def4;
  }
  .sk_theme .feature_name {
    color: #e0def4;
  }
  .sk_theme .separator {
    color: #524f67;
  }
  body {
    margin: 0;

    font-family: "JetBrains Mono NL", "Cascadia Code", "Helvetica Neue", Helvetica, Arial, sans-serif;
    font-size: 12px;
  }
  #sk_omnibar {
    overflow: hidden;
    position: fixed;
    width: 80%;
    max-height: 80%;
    left: 10%;
    text-align: left;
    box-shadow: 0px 2px 10px #21202e;
    z-index: 2147483000;
  }
  .sk_omnibar_middle {
    top: 10%;
    border-radius: 4px;
  }
  .sk_omnibar_bottom {
    bottom: 0;
    border-radius: 4px 4px 0px 0px;
  }
  #sk_omnibar span.omnibar_highlight {
    text-shadow: 0 0 0.01em;
  }
  #sk_omnibarSearchArea .prompt, #sk_omnibarSearchArea .resultPage {
    display: inline-block;
    font-size: 20px;
    width: auto;
  }
  #sk_omnibarSearchArea>input {
    display: inline-block;
    width: 100%;
    flex: 1;
    font-size: 20px;
    margin-bottom: 0;
    padding: 0px 0px 0px 0.5rem;
    background: transparent;
    border-style: none;
    outline: none;
  }
  #sk_omnibarSearchArea {
    display: flex;
    align-items: center;
    border-bottom: 1px solid #524f67;
  }
  .sk_omnibar_middle #sk_omnibarSearchArea {
    margin: 0.5rem 1rem;
  }
  .sk_omnibar_bottom #sk_omnibarSearchArea {
    margin: 0.2rem 1rem;
  }
  .sk_omnibar_middle #sk_omnibarSearchResult>ul {
    margin-top: 0;
  }
  .sk_omnibar_bottom #sk_omnibarSearchResult>ul {
    margin-bottom: 0;
  }
  #sk_omnibarSearchResult {
    max-height: 60vh;
    overflow: hidden;
    margin: 0rem 0.6rem;
  }
  #sk_omnibarSearchResult:empty {
    display: none;
  }
  #sk_omnibarSearchResult>ul {
    padding: 0;
  }
  #sk_omnibarSearchResult>ul>li {
    padding: 0.2rem 0rem;
    display: block;
    max-height: 600px;
    overflow-x: hidden;
    overflow-y: auto;
  }
  .sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #1f1d2e;
  }
  .sk_theme #sk_omnibarSearchResult>ul>li.focused {
    background: #26233a;
  }
  .sk_theme #sk_omnibarSearchResult>ul>li.window {
    border: 2px solid #524f67;
    border-radius: 8px;
    margin: 4px 0px;
  }
  .sk_theme #sk_omnibarSearchResult>ul>li.window.focused {
    border: 2px solid #c4a7e7;
  }
  .sk_theme div.table {
    display: table;
  }
  .sk_theme div.table>* {
    vertical-align: middle;
    display: table-cell;
  }
  #sk_omnibarSearchResult li div.title {
    text-align: left;
  }
  #sk_omnibarSearchResult li div.url {
    font-weight: bold;
    white-space: nowrap;
  }
  #sk_omnibarSearchResult li.focused div.url {
    white-space: normal;
  }
  #sk_omnibarSearchResult li span.annotation {
    float: right;
  }
  #sk_omnibarSearchResult .tab_in_window {
    display: inline-block;
    padding: 5px;
    margin: 5px;
    box-shadow: 0px 2px 10px #21202e;
  }
  #sk_status {
    position: fixed;
    bottom: 0;
    right: 20%;
    z-index: 2147483000;
    padding: 4px 8px 0 8px;
    border-radius: 4px 4px 0px 0px;
    border: 1px solid #524f67;
    font-size: 12px;
  }
  #sk_status>span {
    line-height: 16px;
  }
  .expandRichHints span.annotation {
    padding-left: 4px;
    color: #ebbcba;
  }
  .expandRichHints .kbd-span {
    min-width: 30px;
    text-align: right;
    display: inline-block;
  }
  .expandRichHints kbd>.candidates {
    color: #e0def4;
    font-weight: bold;
  }
  .expandRichHints kbd {
    padding: 1px 2px;
  }
  #sk_find {
    border-style: none;
    outline: none;
  }
  #sk_keystroke {
    padding: 6px;
    position: fixed;
    float: right;
    bottom: 0px;
    z-index: 2147483000;
    right: 0px;
    background: #191724;
    color: #e0def4;
  }
  #sk_usage, #sk_popup, #sk_editor {
    overflow: auto;
    position: fixed;
    width: 80%;
    max-height: 80%;
    top: 10%;
    left: 10%;
    text-align: left;
    box-shadow: #21202e;
    z-index: 2147483298;
    padding: 1rem;
  }
  #sk_nvim {
    position: fixed;
    top: 10%;
    left: 10%;
    width: 80%;
    height: 30%;
  }
  #sk_popup img {
    width: 100%;
  }
  #sk_usage>div {
    display: inline-block;
    vertical-align: top;
  }
  #sk_usage .kbd-span {
    width: 80px;
    text-align: right;
    display: inline-block;
  }
  #sk_usage .feature_name {
    text-align: center;
    padding-bottom: 4px;
  }
  #sk_usage .feature_name>span {
    border-bottom: 2px solid #524f67;
  }
  #sk_usage span.annotation {
    padding-left: 32px;
    line-height: 22px;
  }
  #sk_usage * {
    font-size: 10pt;
  }
  kbd {
    white-space: nowrap;
    display: inline-block;
    padding: 3px 5px;
    font: 11px "JetBrains Mono NL", "Cascadia Code", "Helvetica Neue", Helvetica, Arial, sans-serif;
    line-height: 10px;
    vertical-align: middle;
    border: solid 1px #524f67;
    border-bottom-lolor: #524f67;
    border-radius: 3px;
    box-shadow: inset 0 -1px 0 #21202e;
  }
  #sk_banner {
    padding: 0.5rem;
    position: fixed;
    left: 10%;
    top: -3rem;
    z-index: 2147483000;
    width: 80%;
    border-radius: 0px 0px 4px 4px;
    border: 1px solid #524f67;
    border-top-style: none;
    text-align: center;
    background: #191724;
    white-space: nowrap;
    text-overflow: ellipsis;
    overflow: hidden;
  }
  #sk_tabs {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: transparent;
    overflow: auto;
    z-index: 2147483000;
  }
  div.sk_tab {
    display: inline-block;
    border-radius: 3px;
    padding: 10px 20px;
    margin: 5px;
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#191724), color-stop(100%,#191724));
    box-shadow: 0px 3px 7px 0px #21202e;
  }
  div.sk_tab_wrap {
    display: inline-block;
  }
  div.sk_tab_icon {
    display: inline-block;
    vertical-align: middle;
  }
  div.sk_tab_icon>img {
    width: 18px;
  }
  div.sk_tab_title {
    width: 150px;
    display: inline-block;
    vertical-align: middle;
    font-size: 10pt;
    white-space: nowrap;
    text-overflow: ellipsis;
    overflow: hidden;
    padding-left: 5px;
    color: #e0def4;
  }
  div.sk_tab_url {
    font-size: 10pt;
    white-space: nowrap;
    text-overflow: ellipsis;
    overflow: hidden;
    color: #c4a7e7;
  }
  div.sk_tab_hint {
    display: inline-block;
    float:right;
    font-size: 10pt;
    font-weight: bold;
    padding: 0px 2px 0px 2px;
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#191724), color-stop(100%,#191724));
    color: #e0def4;
    border: solid 1px #524f67;
    border-radius: 3px;
    box-shadow: #21202e;
  }
  #sk_bubble {
    position: absolute;
    padding: 9px;
    border: 1px solid #524f67;
    border-radius: 4px;
    box-shadow: 0 0 20px #21202e;
    color: #e0def4;
    background-color: #191724;
    z-index: 2147483000;
    font-size: 14px;
  }
  #sk_bubble .sk_bubble_content {
    overflow-y: scroll;
    background-size: 3px 100%;
    background-position: 100%;
    background-repeat: no-repeat;
  }
  .sk_scroller_indicator_top {
    background-image: linear-gradient(#191724, transparent);
  }
  .sk_scroller_indicator_middle {
    background-image: linear-gradient(transparent, #191724, transparent);
  }
  .sk_scroller_indicator_bottom {
    background-image: linear-gradient(transparent, #191724);
  }
  #sk_bubble * {
    color: #e0def4 !important;
  }
  div.sk_arrow>div:nth-of-type(1) {
    left: 0;
    position: absolute;
    width: 0;
    border-left: 12px solid transparent;
    border-right: 12px solid transparent;
    background: transparent;
  }
  div.sk_arrow[dir=down]>div:nth-of-type(1) {
    border-top: 12px solid #524f67;
  }
  div.sk_arrow[dir=up]>div:nth-of-type(1) {
    border-bottom: 12px solid #524f67;
  }
  div.sk_arrow>div:nth-of-type(2) {
    left: 2px;
    position: absolute;
    width: 0;
    border-left: 10px solid transparent;
    border-right: 10px solid transparent;
    background: transparent;
  }
  div.sk_arrow[dir=down]>div:nth-of-type(2) {
    border-top: 10px solid #e0def4;
  }
  div.sk_arrow[dir=up]>div:nth-of-type(2) {
    top: 2px;
    border-bottom: 10px solid #e0def4;
  }
  .ace_editor.ace_autocomplete {
    z-index: 2147483300 !important;
    width: 80% !important;
  }
  @media only screen and (max-width: 767px) {
    #sk_omnibar {
      width: 100%;
      left: 0;
    }
    #sk_omnibarSearchResult {
      max-height: 50vh;
      overflow: scroll;
    }
    .sk_omnibar_bottom #sk_omnibarSearchArea {
      margin: 0;
      padding: 0.2rem;
    }
  }
`;
