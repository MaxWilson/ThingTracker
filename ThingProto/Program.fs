open Microsoft.IdentityModel.Clients.ActiveDirectory
open System
open System.Net.Http
open System.Net
open System.Net.Http.Headers
open Newtonsoft.Json
open System.Linq
open System.Net.Http

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let auth =
  let ctx = AuthenticationContext("https://login.microsoftonline.com/wilsonmaxgmail.onmicrosoft.com");
  let resId = "aa5bb4b3-84f1-4b8e-88f8-ea1d14ca1e41"
  let clientId = "6e316726-7405-4cd7-a8b4-d7f2e94cbe36"
  let tok =
    // only pop up browser window if silent token acquisition fails
    try
      ctx.AcquireTokenAsync(resId, clientId, Uri("https://WilsonClient"), new PlatformParameters(PromptBehavior.Never)).Result
    with e ->
      ctx.AcquireTokenAsync(resId, clientId, Uri("https://WilsonClient"), new PlatformParameters(PromptBehavior.Auto)).Result
  fun (msg: HttpRequestMessage) ->
    msg.Headers.Authorization <- AuthenticationHeaderValue("bearer", tok.AccessToken)
    msg

type ThingTracking = {
  name: string
  instances: DateTime list
  }

let client = new HttpClient()

let (|AnyCase|) (input: string) = input.ToLowerInvariant()

let get<'t> (cmd: string) =
  let resp = client.SendAsync(new HttpRequestMessage(HttpMethod.Get, "https://wilsondata.azurewebsites.net/api/" + cmd) |> auth).Result
  if resp.IsSuccessStatusCode then
    resp.Content.ReadAsStringAsync().Result |> JsonConvert.DeserializeObject<'t> |> Some
  else
    None

let post (cmd: string) (payload:'t) =
  let content = new StringContent(payload |> JsonConvert.SerializeObject)
  let url = "https://wilsondata.azurewebsites.net/api/" + cmd
  let resp = client.SendAsync(new HttpRequestMessage(HttpMethod.Post, url, Content = content) |> auth).Result
  if resp.IsSuccessStatusCode then
    ()
  else
    failwithf "Unexpected error when sending %s: %s" url (resp.StatusCode.ToString())

let render thing =
  let recent = [
    let today = DateTime.Now.Date
    for x in 0. .. 6. do
      let start1, end1 = today.AddDays(-x), today.AddDays(1. - x)
      let count = thing.instances |> List.sumBy (fun dt -> if start1 <= dt && dt <= end1 then 1 else 0)
      yield (sprintf "%s %s %d" (start1.ToShortDateString()) (start1.DayOfWeek.ToString()) count)
    let before = today.AddDays(-6.)
    yield (sprintf "Before %s %d" (before.ToShortDateString())
            (thing.instances |> List.sumBy (fun dt -> if dt < before then 1 else 0)))
    ]
  printfn "%s: \n\t%s" thing.name (System.String.Join(",\n\t", recent))

[<EntryPoint>]
let main argv =
    let thingCmd cmd = cmd + "/thingTracker/"
    let alphanum = System.Text.RegularExpressions.Regex("^[a-zA-Z0-9]*$")
    let rec loop() =
      printf "> "
      match Console.ReadLine().Trim() with
      | AnyCase "q" -> 0
      | "list" ->
        let things = get<ThingTracking[]> (thingCmd "List") |> Option.get
        for thing in things do
          render thing
        loop()
      | AnyCase name when alphanum.IsMatch(name) ->
        let thing =
          match get<ThingTracking> (thingCmd "Load" + name) with
          | Some(v) -> v
          | None -> { name = name; instances = [] }
        post (thingCmd "Save" + name) ({ thing with instances = DateTime.Now :: thing.instances })
        get<ThingTracking> (thingCmd "Load" + name) |> Option.get |> render
        loop()
      | v ->
        printfn "Come again? I didn't understand '%s'." v
        loop()
    loop()
