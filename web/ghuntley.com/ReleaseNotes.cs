using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Octokit;
using Statiq.Common;
using Statiq.Core;
using Statiq.Web.GitHub;
using Statiq.Web.Modules;

namespace Statiqdev
{
    public class ReleaseNotes : Pipeline
    {
        private static readonly string[] Projects = new[] { "Statiq.Framework", "Statiq.Web", "Statiq.Docs" };

        public ReleaseNotes()
        {
            DependencyOf.Add(nameof(Statiq.Web.Pipelines.Content));

            InputModules = new ModuleList
            {
                new ExecuteIf("GITHUB_TOKEN")
                {
                    new ReadGitHub(async (ctx, github) =>
                        (await Projects.ToAsyncEnumerable().SelectManyAwait(x => GetReleaseNotesAsync(github, x)).ToArrayAsync())
                            .ToDocuments(sourceFunc: x => ctx.FileSystem.RootPath / ctx.FileSystem.InputPaths[0] / $"blog/posts/{x.Project}-{x.Name}.md", null))
                        .WithCredentials(Config.FromSetting<string>("GITHUB_TOKEN"))
                }
            };

            ProcessModules = new ModuleList
            {
                // Need to replace "@" for Razor and "<?" because some of the release notes reference shortcode syntax
                new SetContent(Config.FromDocument(doc => doc.GetString(nameof(ReleaseNote.Body)).Replace("@", "@@").Replace("<?", "&lt;?")), MediaTypes.Markdown),
                new SetMetadata(SiteKeys.Topic, Config.FromDocument(doc => "release")),
                new SetMetadata(Keys.Title, Config.FromDocument(doc => $"{doc[nameof(ReleaseNote.Project)]} Release {doc[nameof(ReleaseNote.Name)]}")),
                new SetDestination(Config.FromDocument(doc =>
                    new NormalizedPath($"blog/{doc.GetDateTimeOffset(nameof(ReleaseNote.Published)):yyyy/MM/dd}/{doc[nameof(ReleaseNote.Project)]}-{doc[nameof(ReleaseNote.Name)]}.html")
                        .OptimizeFileName()))
            };
        }

        private async ValueTask<IAsyncEnumerable<ReleaseNote>> GetReleaseNotesAsync(GitHubClient github, string project) =>
            (await github.Repository.Release.GetAll("statiqdev", project)).Where(x => x.PublishedAt.HasValue).Select(x => new ReleaseNote(project, x)).ToAsyncEnumerable();

        private class ReleaseNote
        {
            private readonly Release _release;

            public ReleaseNote(string project, Release release)
            {
                Project = project;
                _release = release;
            }

            public string Project { get; }

            public string Name => _release.Name;

            public string Body => _release.Body;

            public DateTime Published => _release.PublishedAt.Value.DateTime;
        }
    }
}
