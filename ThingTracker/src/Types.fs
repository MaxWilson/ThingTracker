module App.Types

open Global
open System

type ThingTracking = {
  name: string
  instances: DateTimeOffset list
  }

module Auth =
  type Provider = Facebook // TODO: add Google, MSA
  (* Transitions:
     Unitialized (on app start) -> Authenticated | Unauthenticated when initialization completes
     Unauthenticated -> Authenticated when user logs in and Facebook sends a response
     Authenticated -> Authorized when WilsonData EasyAuth provides an X-ZUMO-AUTH token
     Authenticated | Authorized -> Unauthenticated when user logs out
  *)
  type AccessToken = string
  type State =
    | Uninitialized
    | Unauthenticated
    | Authenticated of Provider * AccessToken
    | Authorized of AccessToken
  type Transition =
    | Authenticated of Provider * AccessToken
    | Authorized of AccessToken
    | Unauthenticated
    | Logout

type UIState =
  | Tracking
  | Busy
  | AddingNew of string

type Msg =
  | AuthEvent of Auth.Transition
  | FetchList
  | FetchedList of ThingTracking list
  | AddInstance of string
  | AddTracker of string
  | Input of string
  | GotoAdd
  | Saved of requestTime: DateTimeOffset
  | Undo of string

type ViewModel = {
  routes: UIState list
  savingSince: DateTimeOffset option
  }
type Model = {
  things: ThingTracking list
  viewModel: ViewModel
  auth: Auth.State
  }
