using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Octokit;
using Statiq.App;
using Statiq.Common;
using Statiq.Web;

namespace Statiqdev
{
    public static class Program
    {
        public static async Task<int> Main(string[] args) =>
            await Bootstrapper.Factory
                .CreateWeb(args)
                .AddSetting(Keys.Host, "ghuntley.com")
                .AddSetting(Keys.LinksUseHttps, true)
                .AddSetting(WebKeys.MirrorResources, false)
                .AddSetting(WebKeys.GenerateSitemap, true)
                .AddSetting("EditLink", Config.FromDocument((doc, ctx) => "https://github.com/ghuntley/ghuntley/edit/trunk/web/ghuntley.com/input/" + doc.Source.GetRelativeInputPath()))
                .AddSetting(SiteKeys.NoChildPages, Config.FromDocument(doc => doc.Destination.Segments[0].SequenceEqual("blog".AsMemory())))
                .AddPipelines()
                .RunAsync();
    }
}
