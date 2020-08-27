---
title: Slack
---


# Bulk delete Slack attachments

1. https://github.com/kfei/slack-cleaner
2. https://get.slack.help/hc/en-us/articles/215770388-Create-and-regenerate-API-tokens

```shell
› slack-cleaner --token [token] --file --user "*" --before 20171101

Will delete file -> Welcome to Slack!
Will delete file -> Channels Keep Your Conversations Organized
Will delete file -> Uploading Your Files Into Slack
Will delete file -> Getting Started with Posts

491 file(s) will be cleaned.

Now you can re-run this program with `--perform` to actually perform the task.
```
