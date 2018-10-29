module App.Types

open Global
open System

type ThingTracking = {
  name: string
  instances: DateTimeOffset list
  }

type Msg =
  | FetchList
  | FetchedList of ThingTracking list
  | AddInstance of string
  | AddTracker of string
  | Input of string
  | GotoAdd
  | Saving
  | Saved


type UIState =
  | Tracking
  | AddingNew of string

type Model = {
  things: ThingTracking list
  isBusy: bool
  state: UIState
  }
