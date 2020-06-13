using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Octokit;
using Statiq.App;
using Statiq.Common;
using Statiq.Web;

namespace StatiqWebsite
{
    public static class Program
    {
        public static async Task<int> Main(string[] args) =>
            await Bootstrapper.Factory
                .CreateWeb(args)
                .DeployToNetlify(
                    Environment.GetEnvironmentVariable("NETLIFY_SITE_ID"),
                    Environment.GetEnvironmentVariable("NETLIFY_ACCESS_TOKEN")
                )
                .AddSetting(WebKeys.NetlifyRedirects, false)
                .AddSetting(Keys.Host, "ghuntley.com")
                .AddSetting(Keys.LinksUseHttps, true)
                .AddSetting(WebKeys.MirrorResources, true)
                .AddSetting(WebKeys.GenerateSitemap, true)
                .AddSetting("EditLink", Config.FromDocument((doc, ctx) => "https://github.com/ghuntley/ghuntley/edit/trunk/web/ghuntley.com/input/" + doc.Source.GetRelativeInputPath()))
                .AddPipelines()
                .RunAsync();
    }
}
