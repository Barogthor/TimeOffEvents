module Tests

open System
open System.Net
open System.Net.Http
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open ServerCode.App
open Storage.Events
open TimeOff
open Xunit

// ---------------------------------
// Test server/client setup
// ---------------------------------

let createHost() =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = InMemoryStore.Create<UserId, RequestEvent>()

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
//        .Build()
//        .Run()

// ---------------------------------
// Helper functions
// ---------------------------------

let get (client : HttpClient) (path : string) =
    path
    |> client.GetAsync

let createRequest (method : HttpMethod) (path : string) =
    let url = "http://127.0.0.1" + path
    new HttpRequestMessage(method, url)

let addCookiesFromResponse (response : HttpResponseMessage)
                           (request  : HttpRequestMessage) =
    request.Headers.Add("Cookie", response.Headers.GetValues("Set-Cookie"))
    request

let makeRequest (client : HttpClient) request =
    request
    |> client.SendAsync

let isStatus (code : HttpStatusCode) (response : HttpResponseMessage) =
    Assert.Equal(code, response.StatusCode)
    response

let isOfType (contentType : string) (response : HttpResponseMessage) =
    Assert.Equal(contentType, response.Content.Headers.ContentType.MediaType)
    response

let readText (response : HttpResponseMessage) =
    response.Content.ReadAsStringAsync()

let shouldEqual expected actual =
    Assert.Equal(expected, actual)

// ---------------------------------
// Tests
// ---------------------------------


[<Fact>]
let ``Hello world test`` () =
    task {
        use server = new TestServer(createHost())
        use client = server.CreateClient()
        let! response = get client "/"
        let! content =
            response
            |> isStatus HttpStatusCode.OK
            |> readText
        content
        |> shouldEqual "Hello world!"
    }