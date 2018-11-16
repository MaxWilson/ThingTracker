module App.Types

open System
open Common

module Instance =
  type Data = DateTime * int
  let combine (lst: Data list) =
    lst |> List.groupBy fst |> List.map (fun (dt, data) -> (dt, data |> List.sumBy snd))
  let normalize (lst: Data list) =
    // compensate for weak typing: if Data was from an old version it will actually be DateTimeOffset instead of Data, need to convert
    let convert (d: Data) =
      match box d with
      | :? DateTimeOffset as dt ->
        dt.Date, 1
      | _ -> d
    lst |> List.map convert
  let removeMostRecent (lst: Data list) =
    let (dt, count) = lst |> List.maxBy fst
    let rest = lst |> List.filter (fst >> flip (<) dt)
    if count > 1 then (dt, count - 1) :: rest
    else rest

type ThingTracking = {
  name: string
  instances: Instance.Data list
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
