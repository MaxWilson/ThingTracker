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

let mutable onKeypress = fun (ev: KeyboardEvent) -> false
document.addEventListener_keydown(fun ev -> if onKeypress ev then
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
  R.p [] [
    yield R.div [] [
      R.text [Style [FontWeight "bold"]] [str model.name]
      R.button [OnClick (fun _ -> dispatch <| AddInstance model.name)] [str "+"]
      ]
    yield ul [] [
      for x in recent do
        yield li [] [str x]
      ]
    ]

[<Emit("""
  window.fbAsyncInit = function() {
    FB.init({
      appId      : '2065879493471182',
      cookie     : true,
      xfbml      : true,
      version    : 'v3.2'
    });
      
    FB.AppEvents.logPageView();   
  };

  (function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "https://connect.facebook.net/en_US/sdk.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));
""")>]
let initializeFacebook() = jsNative

let root (model:Model) dispatch =
  let onKeyDown keyCode action =
      OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
          if ev.keyCode = keyCode then
              ev.preventDefault()
              action ev)
  onKeypress <-
    fun e ->
      if e.key = "+" && model.state = Tracking then
        dispatch GotoAdd
        true
      else
        false
  let pageHtml = function
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
        yield button [OnClick (fun _ -> dispatch GotoAdd)] [str "+"]
        yield R.h1 [] [str "Things:"]
        for thing in model.things do
          yield renderThing thing dispatch
        ]

  div
    []
    [
      div [ClassName "fb-login-button"; Data ("max-rows", "1"); Data("size", "medium"); Data("button-type", "login_with"); Data("show-faces", "false"); Data("auto-logout-link", "true"); Data("use-continue-as", "true")] []
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
                    [ pageHtml model.state ] ] ] ] ]

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

initializeFacebook()

