module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Types
open App.State
open Global
module Option = Microsoft.FSharp.Core.Option
module R = Fable.Helpers.React
type DayOfWeek = System.DayOfWeek

importAll "../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props

let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]


module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow =  40.

let mutable onKeypress : (KeyboardEvent -> bool) option = None
document.addEventListener_keydown(fun ev -> if onKeypress.IsSome && onKeypress.Value(ev) then
                                              ev.preventDefault()
                                            obj())

let renderThing (model:ThingTracking) dispatch =
  let recent = [
    let today = System.DateTimeOffset.Now.Date |> System.DateTimeOffset
    for x in 0. .. 6. do
      let start1, end1 = today.AddDays(-x), today.AddDays(1. - x)
      let count = model.instances |> List.sumBy (fun dt -> if start1 <= dt && dt <= end1 then 1 else 0)
      let dayOfWeek = function
      | DayOfWeek.Sunday -> "Sun"
      | DayOfWeek.Monday -> "Mon"
      | DayOfWeek.Tuesday -> "Tue"
      | DayOfWeek.Wednesday -> "Wed"
      | DayOfWeek.Thursday -> "Thu"
      | DayOfWeek.Friday -> "Fri"
      | _Saturday -> "Sat"
      yield (sprintf "%s %s %d" (dayOfWeek start1.DayOfWeek) (start1.ToString("MM/dd")) count)
    let before = today.AddDays(-6.)
    yield (sprintf "Before %s %d" (before.ToString("MM/dd"))
            (model.instances |> List.sumBy (fun dt -> if dt < before then 1 else 0)))
    ]
  R.div [] [
    yield R.div [] [
      R.text [Style [FontWeight "bold"]] [str model.name]
      R.button [OnClick (fun _ -> dispatch <| AddInstance model.name)] [str "+"]
      ]
    yield ul [] [
      for x in recent do
        yield li [] [str x]
      ]
    yield R.br []
    ]

let root (model:Model) dispatch =
  let onKeyDown keyCode action =
      OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
          if ev.keyCode = keyCode then
              ev.preventDefault()
              action ev)
  let handler (e: KeyboardEvent) =
    if e.key = "+" && (match model.viewModel with { routes = Tracking::_ } -> true | _ -> false) then
      dispatch GotoAdd
      true
    else
      false
  onKeypress <- Some handler
  let pageHtml = function
    | Busy ->
      R.h1 [] [str "Loading..."]
    | AddingNew name ->
      div
        [ ]
        [ p
            [ ClassName "control" ]
            [ input
                [ ClassName "input"
                  Type "text"
                  Placeholder "What do you want to track, dude?"
                  DefaultValue name
                  AutoFocus true
                  OnChange (fun ev -> !!ev.target?value |> Input |> dispatch )
                  onKeyDown KeyCode.enter (fun _ -> dispatch (AddTracker name))
                  ] ]
          br [ ]
          button [OnClick (fun _ -> dispatch (AddTracker name))] [str "OK"] ]
    | Tracking ->
      div [ClassName "content"] [
        yield button [OnClick (fun _ -> dispatch GotoAdd)] [str "Add New Thing"]
        yield R.h1 [] [str "Things I'm tracking:"]
        for thing in model.things do
          yield renderThing thing dispatch
        yield button [OnClick (fun _ -> Facebook.Logout dispatch)] [str "Log out"]
        ]

  div
    []
    [
      //div
      //  [ ClassName "navbar-bg" ]
      //  [ div
      //      [ ClassName "container" ]
      //      [ Navbar.View.root ] ]
      div
        [ ClassName "section" ]
        [ div
            [ ClassName "container" ]
            [ div
                [ ClassName "columns" ]
                [ div
                    [ ClassName "column" ]
                    [
                      match model.auth with
                      | Auth.Unauthenticated ->
                        yield button [OnClick (fun _ -> State.Facebook.Login dispatch)] [str "Login with Facebook"]
                      | Auth.Authorized _ ->
                        yield (pageHtml (model.viewModel.routes |> List.tryHead |> Microsoft.FSharp.Core.Option.defaultValue Tracking) )
                      | Auth.Uninitialized ->
                        yield str "Loading..."
                      | Auth.Authenticated _ ->
                        yield str "Logging in..."
                      ] ] ] ] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run


