// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

// Geoffrey Huntley <ghuntley@ghuntley.com> based upon
// send to Gmail plugin by Google Inc (c) 2009.

/**
 * @fileoverview Handles all message communication for the
 * for the background page for the Send from Gmail extension.
 */


var baseUrl = "https://mail.live.com/default.aspx?rru=compose";
// &to="" 
// &cc=""
// &subject=""
// &body=""

var subjectPrefix = '';
var title = '';
var url = '';

// selected text from the current tab passed from content script.
var selectedText = '';

chrome.extension.onRequest.addListener(
  function(connectionInfo) {
    selectedText = connectionInfo;
    makeOutlookWin(selectedText);
});

// From = <whatever gmail account is logged in;
//         If not logged in, redirects to login page>
// To = <Unfilled>
// Subject = [Interesting Page] <Page's Title>
// Body = Summary Selection + URL
chrome.browserAction.onClicked.addListener(
  function(tab) {
    
    title = tab.title;
    url = tab.url;

    chrome.tabs.executeScript(null, {file: "infopasser.js"});
});

function makeOutlookWin(summary) {
  // Ensure this is the active window
  var body = '';
  console.log("Summary = " + summary);
  var subject = "";

  subject += title;
  if (summary == '') {
    body = url;
  } else {
    body = summary + "\n" + url;
  }
  
  var outlookUrl = baseUrl +
    "&subject=" + encodeURIComponent(subject) +
    "&body=" + encodeURIComponent(body);

  chrome.windows.create({
    url: outlookUrl,
    left: 20,
    top: 30,
    width: 700,
    height: 600
    });
}

