module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open System

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

let init result =
  {
    things = []
    isBusy = true
    state = UIState.Tracking
    }, Cmd.ofMsg FetchList

let update msg model =
  match msg with
  | FetchList ->
    { model with isBusy = true }, Cmd.Empty
  | FetchedList lst ->
     { model with isBusy = false; things = lst }, Cmd.Empty
  | AddInstance name ->
    let update recognizer transform lst =
      lst |> List.map(fun i -> if (recognizer i) then transform(i) else i)
    { model with things = model.things |> update (fun t -> t.name = name) (fun t -> { t with instances = DateTimeOffset.Now :: t.instances }) }, Cmd.Empty
  | AddTracker name ->
    { model with things = { name = name; instances = [] } :: model.things; state = Tracking }, Cmd.Empty
  | GotoAdd ->
    { model with state = AddingNew "" }, Cmd.Empty
  | Input txt ->
    { model with state = AddingNew txt }, Cmd.Empty

