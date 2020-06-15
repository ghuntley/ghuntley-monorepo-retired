---
layout: notes
title: refit
---

[Refit is a super-cool library](https://github.com/paulcbetts/refit) that is an absolute must have when doing Xamarin or .NET development in general. Personally I would put it right up there with ReSharper, as in if you're not using it then you're stealing time and money from clients. Refit converts a simple interface/contract that [has been decorated with the appropriate attributes](https://github.com/paulcbetts/refit#api-attributes) and automatically converts it into a live REST API client.

Today I had a wire up a Xamarin application into a REST API that was supplied by a 3rd party vendor.

    GET - /packagegrab/?transform.xmlrss={identifier}

Sounds innocent enough right? Nope. The implementation sends back well-formed json but it is contained within a zip archive. Additionally to make matters worse the server has `mod_gzip` enabled which means that the response is double compressed - something that actually increases the size of the response/consumes more bandwidth.

<img src="https://i.imgur.com/L2ORolu.gif"/><br/>
<i>RESTful API? More like STRESSfulAPI.</i>


Now by default Refit will automatically deserialize the response returned from [Microsoft.Net.Http](https://www.nuget.org/packages/Microsoft.Net.Http/) into the approprate type as specified on the interface. This automatic behaviour can be disabled by specifying `HttpResponseMessage` as the return type which will instead return the raw response.

Thanks to the magical powers of [extension methods on interfaces](https://stackoverflow.com/questions/2770333/can-extension-methods-be-applied-to-interfaces) Refit can be extended to add additional methods to the API client such as exposing a method that unwraps the archive and returns a perfectly deserialized object.

    [Headers("Accept: application/json")]
    public interface ITableFlipApi
    {
        [Get("/packagegrab/?transform.xmlrss={identifier}")]
        Task<HttpResponseMessage> GetNewsFeedZipArchive(int identifier);
    }

    public static class ITableFlipApiExtensions
    {
        public static async Task<NewsFeed> GetNewsFeed(this ITableFlipApi This, int identifier)
        {
            var response = await This.GetNewsFeedZipArchive(identifier);

            // throw exception if the request was unsuccessful. (ie. anything other than http 2xx)
            response.EnsureSuccessStatusCode();

            using (var zipArchive = new ZipArchive(await response.Content.ReadAsStreamAsync(), ZipArchiveMode.Read))
            {
                var json = ExtractNewsFeedJson(zipArchive);
                return JsonConvert.DeserializeObject<NewsFeed>(json);
            }
        }

        private static string ExtractNewsFeedJson(ZipArchive zipArchive)
        {
            Condition.Requires(zipArchive).IsNotNull();
            
            string results;
            
            // a well-formed archive contains a single entry at position 0
            // and the filename is usually is `news.json` but the filename is
            // irrelevant. Always grab the first archive entry, an exception will
            // be thrown if more than one file is found or no files are found.
            var compressedFile = zipArchive.Entries.Single();
            using (var memoryStream = new StreamReader(compressedFile.Open()))
            {
                results = memoryStream.ReadToEnd();
            }
            return results;
        }
    }

That's it! All of that dirty API complexity is now hidden away from your ViewModels and the data can now be cleanly consumed via the extension method:

    var client = RestService.For<ITableFlipApi>("https://api.dev/");
    NewsFeed newsFeed = await client.GetNewsFeed(1234);

The following libraries were used/abused to achieve this outcome:

    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.IO.Compression;
    using System.Linq;
    using System.Net.Http;
    using System.Threading.Tasks;
    using Conditions;
    using Newtonsoft.Json;
    using Refit;

The above implementation is cross-platform compatible and can be stored within your `MyCoolApp.Core` portable class library. If it doesn't work make sure your core library is configured to target `Profile259`.
