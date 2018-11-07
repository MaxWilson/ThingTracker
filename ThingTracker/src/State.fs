module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open System
open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types
open Fable.Core
open System.Net.Security
open Fable.Core
open JsInterop

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

let init result =
  {
    things = []
    viewModel = { routes = [Busy; Tracking] }
    auth = Auth.Uninitialized
    }, Cmd.ofSub (fun dispatch ->
        Facebook.initializeFacebook(Facebook.onAuth dispatch)
        ())

let update msg model =
  //let saveThing (thing: ThingTracking) =
  //  Fable.powerPack.Fetch.postAs
  match msg with
  | FetchList ->
    let fetch() = Fable.PowerPack.Fetch.fetchAs<ThingTracking[]> "https://wilsondata.azurewebsites.net/api/List/thingTracker" [RequestProperties.Credentials RequestCredentials.Include]
    let onSuccess (things: ThingTracking[]) =
      FetchedList (List.ofArray things)
    let onFail (e:Exception) = FetchedList []
    { model with viewModel = { model.viewModel with routes = Busy :: model.viewModel.routes } }, Cmd.ofPromise fetch () onSuccess onFail
  | FetchedList lst ->
     { model with viewModel = { model.viewModel with routes = model.viewModel.routes }; things = lst }, Cmd.Empty
  | AddInstance name ->
    let update recognizer transform lst =
      lst |> List.map(fun i -> if (recognizer i) then transform(i) else i)
    { model with things = model.things |> update (fun t -> t.name = name) (fun t -> { t with instances = DateTimeOffset.Now :: t.instances }) }, Cmd.Empty
  | AddTracker name ->
    { model with things = { name = name; instances = [] } :: model.things; viewModel = { routes = [Tracking] } }, Cmd.Empty
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
      let fetch() = Fable.PowerPack.Fetch.postRecord "https://wilsondata.azurewebsites.net/.auth/login/facebook/callback" (createObj ["accessToken" ==> token])[]
      let model = { model with viewModel = { model.viewModel with routes = Busy :: model.viewModel.routes } }
      model, Cmd.ofPromise fetch () (fun _ -> FetchedList []) (fun _ -> FetchedList [])
    | Auth.Transition.Unauthenticated ->
      { model with auth = Auth.Unauthenticated }, Cmd.Empty
    | event ->
      failwithf "TODO, not implemented %A" event

