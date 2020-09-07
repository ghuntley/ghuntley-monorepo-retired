// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

// Geoffrey Huntley <ghuntley@ghuntley.com> based upon
// send to Gmail plugin by Google Inc (c) 2009.

/**
 * @fileoverview Rewrites the mailto links on the current page
 * to Outlook.com Compose links.
 */
var toField = "&to=";
var baseUrl = "";

function rewriteMailtoToOutlookUrl(inUrl) {
  var retUrl = inUrl;
  retUrl = retUrl.replace("?", "&");
  retUrl = retUrl.replace(/subject=/i, "subject=");
  retUrl = retUrl.replace(/CC=/i, "cc=");
  retUrl = retUrl.replace(/BCC=/i, "bcc=");
  retUrl = retUrl.replace(/Body=/i, "body=");
  var gmailUrl = baseUrl + toField;
  retUrl = retUrl.replace("mailto:", gmailUrl);
  return retUrl;
}

// Content Scripts
function rewriteMailtosOnPage() {
  // Find all the mailto links.
  var result = document.evaluate(
      '//a[contains(@href, "mailto:")]',
      document, null, XPathResult.UNORDERED_NODE_ITERATOR_TYPE, null);

  var item;
  var nodes = [];
  // cannot change the NODE_ITERATOR nodes' attributes in this loop itself
  // since iterateNext will invalidate the state; Need to store temporarily.
  while (item = result.iterateNext()) {
    nodes.push(item);
  }

  for (var i = 0; i < nodes.length; i++) {
    var mailtoStr = nodes[i].getAttribute('href');
    mailtoStr = rewriteMailtoToOutlookUrl(mailtoStr);
    nodes[i].setAttribute('href', mailtoStr);
    nodes[i].setAttribute('target', "_blank");
  }
}

if (baseUrl == "") {
  var baseUrl = "https://mail.live.com/default.aspx?rru=compose";
  rewriteMailtosOnPage();
}

