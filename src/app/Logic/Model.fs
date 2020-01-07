namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | DeclineRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | DeclineCancellationRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | DeclineRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | DeclineCancellationRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestDeclined of TimeOffRequest
    | RequestCancellationDeclined of TimeOffRequest
    | RequestCancellationCreated of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestDeclined request -> request
        | RequestCancellationDeclined request -> request
        | RequestCancellationCreated request -> request
        | RequestCancelled request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    let getCurrentDate () = 
        DateTime.Today
        
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Declined of TimeOffRequest 
        | PendingCancellation of TimeOffRequest 
        | CancellationDeclined of TimeOffRequest 
        | Cancelled of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request
            | Declined request
            | PendingCancellation request
            | CancellationDeclined request
            | Cancelled request -> request
        member this.IsActive =
            match this with
            | Cancelled _
            | Declined _
            | NotCreated -> false
            
            | PendingValidation _
            | Validated _
            | PendingCancellation _ 
            | CancellationDeclined _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestDeclined request -> Declined request
        | RequestCancellationDeclined request -> CancellationDeclined request
        | RequestCancellationCreated request -> PendingCancellation request
        | RequestCancelled request -> Cancelled request
        
    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        let overlapRange = request1.Start.Date < request2.End.Date && request1.End.Date > request2.Start.Date
        let overlapDay r1 r2 = r1.Start.Date = r2.End.Date && (r1.Start.HalfDay = HalfDay.AM && r2.End.HalfDay = HalfDay.PM )
        overlapRange || (overlapDay request1 request2 || overlapDay request2 request1) 
        
    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let lambda otherRequest = overlapsWith request otherRequest
        Seq.exists lambda otherRequests

    let createRequest activeUserRequests request (now: DateTime) =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= now then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
            
    let declineRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestDeclined request]
        | PendingCancellation request ->
            Ok [RequestCancellationDeclined request]
        | _ ->
            Error "Request cannot be declined"
            
    let cancelRequest requestState user (now: DateTime) =
        match requestState, user with
        | PendingValidation request, Manager
        | PendingCancellation request, Manager
        | CancellationDeclined request, Manager
        | Validated request, Manager ->
            Ok [RequestCancelled request]
  
        | PendingValidation request, Employee _ when request.Start.Date > now  ->
            Ok [RequestCancelled request]
            
        | PendingValidation request, Employee _
        | Validated request, Employee _ ->
            Ok [RequestCancellationCreated request]
            
        | PendingCancellation _, Employee _
        | CancellationDeclined _, Employee _ ->
            Error "Unsupported request as user"
        
        | _ ->
            Error "Request cannot be cancelled"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) (now: DateTime) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | Manager ->
            match command with
            | ValidateRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                validateRequest requestState
                
            | DeclineRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                declineRequest requestState
                
            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancelRequest requestState user now
            | _ -> Error "Unsupported request as manager"

        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request now

            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancelRequest requestState user now
            
            | _ -> Error "Unsupported request as user"
