---

title:      Announcing HttpClientDiagnostics
date:       2016-05-04
summary:    HttpClientDiagnostics is a cross platform, portable class library that provides tracing/logging telemetry of Microsoft.Net.HttpClient requests. 
categories: android ios xamarin logging tracing diagnostics liblog httpclient rest networking  
---

[HttpClientDiagnostics](https://github.com/ghuntley/HttpClientDiagnostics) is a cross platform, portable class library that provides tracing/logging telemetry of [Microsoft.Net.HttpClient](https://www.nuget.org/packages/Microsoft.Net.Http/) requests. In the spirit of "applications decide the logging framework, libraries should play nice" this package does _not_ require any external dependencies or abstractions like Common.Logging. Logging all happens automatically, behind the scenes, without you needing to know how or care how to wire this up thanks to the amazing LibLog. [It’s like magic! Its the future](http://nblumhardt.com/2016/04/which-logging-abstraction-should-i-use/).

## Supported Platforms
* .NET 4.5
* Mono
* Xamarin.iOS
* Xamarin.Android
* Xamarin.Mac
* UWP

## Installation

Installation is done via NuGet:

    Install-Package HttpClientDiagnostics

## Usage

    HttpClient client = new HttpClient(new HttpClientDiagnosticsHandler(new HttpClientHandler()));
    HttpResponseMessage response = await client.GetStringAsync("https://api.duckduckgo.com/?q=apple&format=json");

Which will automatically yield in your application logs:

    2016-05-04 18:45:19.291 +10:00 [Debug] Request: Method: GET, RequestUri: 'https://api.duckduckgo.com/?q=apple&format=json', Version: 1.1, Content: <null>, Headers:
    {
    }
    2016-05-04 18:45:21.264 +10:00 [Debug] Response: StatusCode: 200, ReasonPhrase: 'OK', Version: 1.1, Content: System.Net.Http.StreamContent, Headers:
    {
      Connection: keep-alive
      Strict-Transport-Security: max-age=0
      X-DuckDuckGo-Locale: en_US
      Cache-Control: max-age=1
      Date: Wed, 04 May 2016 08:45:21 GMT
      Server: nginx
      Content-Length: 23528
      Content-Type: application/x-javascript
      Expires: Wed, 04 May 2016 08:45:22 GMT
    }
    2016-05-04 18:45:21.681 +10:00 [Debug] Response Content: {"Result":"<a href=\"https://duckduckgo.com/Apple\">Apple</a>A deciduous tree in the rose family best known for its sweet, pomaceous fruit, the apple.}
    2016-05-04 18:45:22.074 +10:00 [Debug] Response elapsed time: 2750 ms
    2016-05-04 18:45:22.077 +10:00 [Debug] Total elapsed time: 2788 ms

## Advanced Usage

The constructor of `HttpClientDiagnosticsHandler` can take another `HttpMessageHandler` which provides you with a way to chain multiple handlers together. A common scenario would be using [Refit](https://github.com/paulcbetts/refit) to automatically convert interface definitions into REST clients and integrating with [ModernHttpClient](https://github.com/paulcbetts/modernhttpclient) for performance improvements, [Fusillade](https://github.com/paulcbetts/Fusillade) for auto-deduplication of requests/request limiting/request prioritization and then finally your own custom handler that handles REST API authentication, authorization/security in a single and central place. Which looks something like this....

    public interface IDuckDuckGoApi
    {
        [Get("/?q={query}&format=json")]
        IObservable<DuckDuckGoSearchResult> Search(string query);
    }

    public class DuckDuckGoApiService : IDuckDuckGoApiService
    {
        public const string ApiBaseAddress = "https://api.duckduckgo.com";

        private readonly Lazy<IDuckDuckGoApi> _background;
        private readonly Lazy<IDuckDuckGoApi> _speculative;
        private readonly Lazy<IDuckDuckGoApi> _userInitiated;

        public DuckDuckGoApiService(string apiBaseAddress = null, bool enableDiagnostics = false)
        {
            Func<HttpMessageHandler, IDuckDuckGoApi> createClient = innerHandler =>
            {
                HttpMessageHandler handler;

                if (enableDiagnostics)
                {
                    handler = new HttpClientDiagnosticsHandler(innerHandler);
                }
                else
                {
                    handler = messageHandler;
                }

                var client = new HttpClient(handler)
                {
                    BaseAddress = new Uri(apiBaseAddress ?? ApiBaseAddress)
                };

                return RestService.For<IDuckDuckGoApi>(client);
            };

            _background = new Lazy<IDuckDuckGoApi>(() => createClient(
                new RateLimitedHttpMessageHandler(new NativeMessageHandler(), Priority.Background)));

            _userInitiated = new Lazy<IDuckDuckGoApi>(() => createClient(
                new RateLimitedHttpMessageHandler(new NativeMessageHandler(), Priority.UserInitiated)));

            _speculative = new Lazy<IDuckDuckGoApi>(() => createClient(
                new RateLimitedHttpMessageHandler(new NativeMessageHandler(), Priority.Speculative)));
        }

        public IDuckDuckGoApi Background => _background.Value;
        public IDuckDuckGoApi Speculative => _speculative.Value;
        public IDuckDuckGoApi UserInitiated => _userInitiated.Value;
    }
