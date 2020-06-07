---
title: thanks
---

# supporters

If you or your company uses open-source software or appreciates the work I do in open source, please [consider supporting me](/support)! I think it's pretty clear that software actually costs something, and that even though it may be offered freely, somebody is paying the cost. 

You'll be helping to ensure I can spend the time not just fixing bugs, adding features, releasing new versions, but also keeping projects afloat and growing. Think of investing in me not just for the output of my code but my continued role in the open-source ecosystem.

These lovely people do, you should too:

- Anonymous (x2)
- Anonymous from Google 
- Alfie John
- Brendan Forster
- Damian Brady
- Devon Zuegel
- Jeremy Sinclair
- John Szaszvari
- Jon Mark Searle
- Kat Marchán
- Kirill Osenkov
- Tien Phan
- Tod Thomson


# contributors

These lovely people have contributed a fix to my blog. If you want to see yourself in this list, send me a pull request! Every post in my blog has an edit link (the octocat button) that lets you edit the blog post directly in the browser which automatically sends me a pull request.

Or [visit my repository](https://github.com/ghuntley/ghuntley) and send me a pull request the old fashioned way.

<div class="contributors"></div>

<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>

<script>
  $.when(
    $.ajax('https://api.github.com/repos/ghuntley/ghuntley/contributors?per_page=250'),
    $.ajax('https://api.github.com/repos/ghuntley/live/contributors?per_page=250'),
    $.ajax('https://api.github.com/repos/reactiveui/website/contributors?per_page=250'),
    $.ajax('https://api.github.com/repos/reactiveui/reactiveui/contributors?per_page=250'))
  .then(function(websiteData, reactiveUIData) {
    var persons = {};
    var allData = websiteData[0].concat(reactiveUIData[0]);

    for(var i = 0; i < allData.length; i++) {
        persons[allData[i].login] = allData[i];
    }

    var sortedLogins = Object.keys(persons).sort();

    $(sortedLogins).each(function (index, login) {
      var person = persons[login];
      var img = '<img class="contributor" src="' + person.avatar_url + '" />';
      $('.contributors')
        .append('<a class="contributor-name" title="' + person.login + '" href="' + person.html_url + '">' 
                + img + '</a>');
    });
  });
</script>

<style>
 
  #content img {
    width: 48px;
    margin: 5px 5px;
    display: inline-block;
  }
  .contributor {
    border-radius: 730px;
    margin: 10px 10px 0 0;
  }

  .contributor-name {
    border-bottom: none;
  }
</style>
