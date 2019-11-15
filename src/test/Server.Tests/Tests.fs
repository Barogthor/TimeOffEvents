module Tests

open System
open System.Diagnostics
open System.Net
open System.Net.Http
open System.IO
open System.Net
open System.Net.Mime
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open Newtonsoft.Json
open ServerCode.App
open Storage.Events
open TimeOff
open TimeOff.AuthTypes
open Xunit

// ---------------------------------
// Test server/client setup
// ---------------------------------

let getDateFromToday (n: int) =
  Logic.getCurrentDate().Add (TimeSpan.FromDays (float n) )


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
    
let deserialize<'a> (content : string) : 'a =
    JsonConvert.DeserializeObject<'a> content
    
let shouldEqual (expected: 'a) (actual: 'a) =
    Assert.Equal<'a>(expected, actual)

let shouldNotEqual (expected: 'a) (actual: 'a) =
    Assert.NotEqual<'a>(expected, actual)

let getUserAuthentified content : UserData =
    JsonConvert.DeserializeObject<UserData> content
    
    
let get (client : HttpClient) (path : string) =
    path
    |> client.GetAsync

let post (client : HttpClient) (path : string) (data : string) =
    let httpContent = new StringContent(data);
    client.PostAsync(path, httpContent)

let postAuth (client: HttpClient) (path : string) (data : string) (jwt : string) =
    let httpContent = new StringContent(data);
//    httpContent.Headers.TryAddWithoutValidation("Authorization", ("Bearer "+jwt))
    client.DefaultRequestHeaders.Add("Authorization", "Bearer "+jwt)
    client.PostAsync(path, httpContent)

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

type Credentials = {
    UserName: string
    Password: string
}

[<Fact>]
let ``Login successful`` () =
    task {
        use server = new TestServer(createHost())
        use client = server.CreateClient()
        let name = "employee1" 
        let login = {
            UserName = name
            Password = name
        }
        let json = JsonConvert.SerializeObject login
        let! response = post client "/api/users/login" json
        let! content =
            response
            |> isStatus HttpStatusCode.OK
            |> readText
            
        let result = content |> deserialize<UserData>
        
        result.UserName
        |> shouldEqual name
        result.User
        |>shouldEqual (Employee name)
        
    }
    
[<Fact>]
let ``Login unsuccessful`` () =
    task {
        use server = new TestServer(createHost())
        use client = server.CreateClient()
        let login = {
            UserName = "john"
            Password = "john"
        }
        let json = JsonConvert.SerializeObject login
        let! response = post client "/api/users/login" json
        let! content =
            response
            |> isStatus HttpStatusCode.Unauthorized
            |> readText 
        content
        |> shouldEqual (sprintf "\"User '%s' can't be logged in.\"" login.UserName)
        
    }
    
//TODO
[<Fact>]
let ``Request timeoff`` () =
    task {
        use server = new TestServer(createHost())
        use client = server.CreateClient()
        let name = "employee1"
        let login = {
            UserName = name
            Password = name
        }
        let json = JsonConvert.SerializeObject login
        let! response = post client "/api/users/login" json
        let! content =
            response
            |> isStatus HttpStatusCode.OK
            |> readText
            
        let user = content |> deserialize<UserData>
        let tomorrow = getDateFromToday 1
        let request = {
            UserId = name
            RequestId = Guid.NewGuid()
            Start = { Date = tomorrow; HalfDay = AM }
            End = { Date = tomorrow; HalfDay = PM }
        }
        let json = JsonConvert.SerializeObject request
//        user.Token
//        |> shouldEqual ""
        let! response = postAuth client "/api/timeoff/request" json user.Token
        let! content =
            response
            |> isStatus HttpStatusCode.OK
            |> readText
        
        content |> shouldNotEqual ""
        
    }