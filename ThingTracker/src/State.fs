module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Types
open System
open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types
open Fable.Core
open System.Net.Security
open Fable.Core
open JsInterop
open Fable.PowerPack

//let pageParser: Parser<Page->Page,Page> =
//  oneOf [
//    map About (s "about")
//    map Counter (s "counter")
//    map Home (s "home")
//  ]

//let urlUpdate (result: Option<Page>) model =
//  match result with
//  | None ->
//    console.error("Error parsing url")
//    model,Navigation.modifyUrl (toHash model.currentPage)
//  | Some page ->
//      { model with currentPage = page }, []

module EasyAuth =
  type Response = { authenticationToken: string }
  let ofFacebook token =
    promise {
      let! resp = Fable.PowerPack.Fetch.postRecord "https://wilsondata.azurewebsites.net/.auth/login/facebook" (createObj ["access_token" ==> token])[]
      if resp.Ok then
        try
          let! retval = resp.json<Response>()
          return retval.authenticationToken
        with e ->
          return e.ToString()
      else
        failwithf "Could not authorize"
        return "Shouldn't get here"
    }

module Facebook =
  type AuthStatus = {
    accessToken: string
    }
  type AuthResponse = {
    authResponse: AuthStatus
    status: string
    }
  let (|AuthToken|_|) = function
    | { status = "connected"; authResponse = { accessToken = accessToken } } -> Some accessToken
    | _ -> None
  let onAuth dispatch = function
    | AuthToken token ->
      dispatch (AuthEvent <| Auth.Transition.Authenticated (Auth.Provider.Facebook, token))
    | _ ->
      dispatch (AuthEvent Auth.Transition.Unauthenticated)
  [<Emit("FB.login($0)")>]
  let private FBLogin handler = jsNative
  let Login dispatch =
    FBLogin(onAuth dispatch) |> ignore
  [<Emit("FB.logout($0)")>]
  let private FBLogout handler = jsNative
  let Logout dispatch =
    FBLogin(fun _ -> dispatch (AuthEvent <| Auth.Transition.Unauthenticated)) |> ignore
  [<Emit("""
    window.fbAsyncInit = function() {
      FB.init({
        appId      : '2065879493471182',
        cookie     : true,
        xfbml      : true,
        version    : 'v3.2'
      });

      FB.AppEvents.logPageView();
      FB.getLoginStatus(resp => $0(resp))
    };

    (function(d, s, id){
       var js, fjs = d.getElementsByTagName(s)[0];
       if (d.getElementById(id)) {return;}
       js = d.createElement(s); js.id = id;
       js.src = "https://connect.facebook.net/en_US/sdk.js";
       fjs.parentNode.insertBefore(js, fjs);
     }(document, 'script', 'facebook-jssdk'));
  """)>]
  let initializeFacebook(_onAuth:AuthResponse -> unit) = jsNative

let thingUrl = sprintf "https://wilsondata.azurewebsites.net/api/thingTracker"
let listUrl = thingUrl
let saveUrl name = sprintf "%s/%s" thingUrl name

let init result =
  {
    things = []
    viewModel = { routes = [Busy; Tracking]; savingSince = None }
    auth = Auth.Uninitialized
    }, Cmd.ofSub (fun dispatch ->
        let warmupAzureFunction() =
          // try to ensure that Azure function is already awake by making a dummy call to auth and list endpoints.
          // otherwise, it can take about five seconds for each to warm up
          Fable.PowerPack.Fetch.fetch "https://wilsondata.azurewebsites.net/.auth/login/me" [] |> ignore
          Fable.PowerPack.Fetch.fetch listUrl [] |> ignore
        Facebook.initializeFacebook(Facebook.onAuth dispatch)
        ())

[<Emit("undefined")>]
let undefined<'t> : 't = jsNative

let update msg model =
  //let saveThing (thing: ThingTracking) =
  //  Fable.powerPack.Fetch.postAs
  match msg with
  | FetchList ->
    let token = match model.auth with Auth.Authorized token -> token | _ -> failwith "Unexpected error: Unauthorized fetch"
    let fetch() = Fable.PowerPack.Fetch.fetchAs<obj> listUrl [Fetch.requestHeaders [HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
    let onSuccess (things) =
      let things = things |> unbox<ThingTracking[]> |> Array.map (fun thing ->
        let fixup (instances: (DateTime * int) list) =
          match instances with
          | h::t when (unbox<obj[]> h).Length = 2 -> // if it is a (dateTime,count) tuple already, skip fixup
            instances
          | _ ->
            instances |> List.map (fun ((d,count) as row) ->
              match DateTime.TryParse (row.ToString()) with
              | true, d -> d, 1
              | _ -> (DateTime.Parse(d.ToString())), count
              )
            |> List.groupBy fst
            |> List.map (fun (d, rows) -> d.Date, rows |> List.sumBy (snd))
        { thing with instances = thing.instances |> fixup }
        )
      FetchedList (List.ofArray things)
    let onFail (e:Exception) = FetchedList []
    { model with viewModel = { model.viewModel with routes = Busy :: model.viewModel.routes } }, Cmd.ofPromise fetch () onSuccess onFail
  | FetchedList lst ->
    { model with viewModel = { model.viewModel with routes = [Tracking] }; things = lst }, Cmd.Empty
  | Saved t ->
    match model.viewModel.savingSince with
      | Some lasttime when lasttime > t -> model, Cmd.Empty
      | _ ->
        // if we're the last request, zero out save time so the UI will blank the "Busy saving..." dialog
        { model with viewModel = { model.viewModel with savingSince = None } }, Cmd.ofMsg FetchList
  | AddInstance name ->
    let update recognizer transform lst =
      lst |> List.map(fun i -> if (recognizer i) then transform(i) else i)
    let things = model.things |> update (fun t -> t.name = name) (fun t -> { t with instances = (DateTimeOffset.Now.Date, 1) :: t.instances })
    let thing = things |> List.find (fun t -> t.name = name)
    let token = match model.auth with Auth.Authorized token -> token | _ -> failwith "Unexpected error: Unauthorized fetch"
    let time = DateTimeOffset.Now
    let req = Fable.PowerPack.Fetch.postRecord<ThingTracking> (saveUrl name) thing [Fetch.requestHeaders [HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
    { model with things = things; viewModel = { model.viewModel with savingSince = Some time } }, Cmd.ofPromise (fun () -> req) () (fun _ -> Saved time) (fun _ -> Saved time)
  | Undo name ->
    // undo something from today
    let update recognizer transform lst =
      lst |> List.map(fun i -> if (recognizer i) then transform(i) else i)
    let token = match model.auth with Auth.Authorized token -> token | _ -> failwith "Unexpected error: Unauthorized fetch"
    let time = DateTimeOffset.Now
    if (model.things |> List.find (fun t -> t.name = name)).instances.IsEmpty then
      let req = Fable.PowerPack.Fetch.fetch (saveUrl name) [Method HttpMethod.DELETE; Fetch.requestHeaders [HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
      { model with viewModel = { model.viewModel with savingSince = Some time } }, Cmd.ofPromise (fun () -> req) () (fun _ -> Saved time) (fun _ -> Saved time)
    else
      let things = model.things |> update (fun t -> t.name = name) (fun t ->
        let time = DateTimeOffset.Now.Date
        let rec removeMostRecent = function
          | (h,count)::rest when h >= time && count > 1 -> (h, count-1)::rest
          | (h,count)::rest when h >= time -> rest
          | h::rest -> h::(removeMostRecent rest)
          | [] -> []
        { t with instances = removeMostRecent t.instances }
        )
      let thing = things |> List.find (fun t -> t.name = name)
      let req = Fable.PowerPack.Fetch.postRecord<ThingTracking> (saveUrl name) thing [Fetch.requestHeaders [HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
      { model with things = things; viewModel = { model.viewModel with savingSince = Some time } }, Cmd.ofPromise (fun () -> req) () (fun _ -> Saved time) (fun _ -> Saved time)
  | AddTracker name ->
    let token = match model.auth with Auth.Authorized token -> token | _ -> failwith "Unexpected error: Unauthorized fetch"
    let thing = { name = name; instances = [] }
    let time = DateTimeOffset.Now
    let req = Fable.PowerPack.Fetch.postRecord<ThingTracking> (saveUrl name) thing [Fetch.requestHeaders [HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
    { model with things = thing :: model.things; viewModel = { savingSince = Some time; routes = [Tracking] } }, Cmd.ofPromise (fun () -> req) () (fun _ -> Saved time) (fun _ -> Saved time)
  | GotoAdd ->
    { model with viewModel = { model.viewModel with routes = (AddingNew "" )::model.viewModel.routes } }, Cmd.Empty
  | Input txt ->
    match model.viewModel.routes with
    | AddingNew _::rest ->
      { model with viewModel = { model.viewModel with routes = (AddingNew txt)::rest } }, Cmd.Empty
    | rest ->
      console.log("Something went wrong, shouldn't get here")
      { model with viewModel = { model.viewModel with routes = (AddingNew txt)::rest } }, Cmd.Empty // handle it anyway
  | AuthEvent ev ->
    match ev with
    | Auth.Transition.Authenticated(Auth.Provider.Facebook, token) ->
      let model = { model with viewModel = { model.viewModel with routes = Busy :: model.viewModel.routes } }
      model, Cmd.ofPromise EasyAuth.ofFacebook token (fun token -> AuthEvent (Auth.Transition.Authorized token)) (fun _ -> FetchedList [])
    | Auth.Transition.Unauthenticated ->
      { model with auth = Auth.Unauthenticated }, Cmd.Empty
    | Auth.Transition.Authorized token ->
      { model with auth = Auth.Authorized token }, Cmd.ofMsg FetchList
    | Auth.Transition.Logout ->
      { model with auth = Auth.Unauthenticated }, Cmd.Empty
