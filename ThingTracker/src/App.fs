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
                  Placeholder "What do you want to track?"
                  DefaultValue name
                  AutoFocus true
                  OnChange (fun ev -> !!ev.target?value |> Input |> dispatch )
                  onKeyDown KeyCode.enter (fun _ -> dispatch (AddTracker name))
                  ] ]
          br [ ]
          button [OnClick (fun _ -> dispatch (AddTracker name))] [str "OK"] ]
    | Tracking ->
      div [] [
        yield button [OnClick (fun _ -> dispatch GotoAdd)] [str "+"]
        yield div[] [str "Things:"]
        for x in model.things do
          yield div[] [str x.name]
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
