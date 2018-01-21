namespace TimeOff

open System
open EventStorage
open Suave
open Suave.Operators
open Suave.Http
open Suave.Successful
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Suave
open Suave.Operators
open Suave.Http
open Suave.Successful

type Repository<'a> = {
    Create : 'a -> 'a
}


type User =
    | Employee of int
    | Manager

type HalfDay = | AM | PM

type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

type UserId = int

type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId

type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request

module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true

    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request

    let getRequestState events =
        events |> Seq.fold evolve NotCreated

    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

    let overlapWithAnyRequest (previousRequests: TimeOffRequest seq) (request: TimeOffRequest) =
        let mutable returnValue = false
        for previousRequest in previousRequests do
            if request.Start.Date > previousRequest.Start.Date && request.Start.Date < previousRequest.End.Date then
                returnValue <- true
            elif request.Start.Date.Equals previousRequest.End.Date && request.Start.HalfDay.Equals previousRequest.End.HalfDay then
                returnValue <- true
            elif request.Start.Date.Equals previousRequest.Start.Date && not (previousRequest.End.Date.Equals previousRequest.Start.Date) then
                returnValue <- true
                
        returnValue

    let createRequest previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let handleCommand (store: IStore<UserId, RequestEvent>) (command: Command) =
        let userId = command.UserId
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        let userRequests = getAllRequests events

        match command with
        | RequestTimeOff request ->
            let activeRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)

            createRequest activeRequests request

        | ValidateRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            validateRequest requestState

module JsonConvert =
    let JSON v =
        let jsonSerializerSettings = new JsonSerializerSettings()
        jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

        JsonConvert.SerializeObject(v, jsonSerializerSettings)
        |> OK
        >=> Writers.setMimeType "application/json; charset=utf-8"

    let fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

module Router =
    open Suave.RequestErrors
    open Suave.Filters
    open Suave.Utils.Collections

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> JsonConvert.fromJson<'a>
    
    let rest resourceName repository =

        let resourcePath = "/" + resourceName
        let resourceIdPath = new PrintfFormat<(int -> string),unit,string,string,int>(resourcePath + "/%d")

        let badRequest = BAD_REQUEST "Resource not found"

      

        choose [
            path resourcePath >=> choose [
                POST >=> request (getResourceFromReq >> repository.Create >> JsonConvert.JSON)
            ]
        ]

module Db =
    let store = InMemoryStore.Create<UserId, RequestEvent>()

    let createRequest request =
        let newRequest = { UserId = request.UserId
                           RequestId = Guid.NewGuid();
                           Start = request.Start
                           End = request.End
                         }
        let command = Command.RequestTimeOff newRequest
        let result = Logic.handleCommand store command
        let seqEvents = seq {
                let command = Command.RequestTimeOff newRequest
                let result = Logic.handleCommand store command
                match result with
                    | Ok events ->
                        for event in events do
                            let stream = store.GetStream event.Request.UserId
                            stream.Append [event]
                            if event.Request.RequestId = newRequest.RequestId then yield event
                    | Error e -> printfn "Error: %s" e
            }
        let result = Seq.toArray seqEvents
        if result.Length > 0 then 
            Console.WriteLine result.[0]
        
        request