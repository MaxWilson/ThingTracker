open Microsoft.IdentityModel.Clients.ActiveDirectory
open System
open System.Net.Http
open System.Net
open System.Net.Http.Headers

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let auth =
  let ctx = AuthenticationContext("https://login.microsoftonline.com/wilsonmaxgmail.onmicrosoft.com");
  let tok = ctx.AcquireTokenAsync("aa5bb4b3-84f1-4b8e-88f8-ea1d14ca1e41", "6e316726-7405-4cd7-a8b4-d7f2e94cbe36", Uri("https://WilsonClient"), new PlatformParameters(PromptBehavior.Auto)).Result;
  fun (msg: HttpRequestMessage) ->
    msg.Headers.Authorization <- AuthenticationHeaderValue("bearer", tok.AccessToken)
    msg

[<EntryPoint>]
let main argv =
    use client = new HttpClient();
    let resp = client.SendAsync(new HttpRequestMessage(HttpMethod.Get, "https://wilsondata.azurewebsites.net/api/List/ssid") |> auth).Result;
    Console.WriteLine(resp.Content.ReadAsStringAsync().Result);

    0 // return an integer exit code
