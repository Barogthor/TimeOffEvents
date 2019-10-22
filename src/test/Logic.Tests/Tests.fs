module TimeOff.Tests

open Expecto
open System
open TimeOff

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)
    
    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
//    System.Console.WriteLine "==============="
//    System.Console.WriteLine globalState
//    System.Console.WriteLine userRequestsState
    Expect.equal result expected message

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }
    
    test "A requests same day overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The 2 requests should overlap for the same day"
    }
    
    test "A requests same day which doesn't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = AM }
      }
      Expect.isFalse (Logic.overlapsWith request1 request2) "The 2 requests shouldn't overlap"
    }
    
    test "requests shouldn't overlaps" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 5); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 5); HalfDay = PM }
      }
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 6); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 6); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The 2 requests shouldn't overlap"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    
    test "Requests on 3 distinct days don't overlap" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let otherRequests = List.toSeq [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 11, 2); HalfDay = AM }
          End = { Date = DateTime(2019, 11, 2); HalfDay = PM }
        }
      ]

      Expect.isFalse (Logic.overlapsWithAnyRequest otherRequests request) "The requests don't overlap"
    }
    
    test "Requests overlaps on a range" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 7); HalfDay = PM }
      }

      let otherRequests = List.toSeq [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 5); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 12); HalfDay = PM }
        };
      ]

      Expect.isTrue (Logic.overlapsWithAnyRequest otherRequests request) "The requests range overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
    
    test "A request isn't overlapping" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request1 ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request2)
      |> Then (Ok [RequestCreated request2]) "The request should have been created"
    }
    
    test "A request is overlapping" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request1 ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request2)
      |> Then (Error "Overlapping request") "The request shouldn't have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let declineTests =
  testList "Declination tests" [
    test "A pending request is declined" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (DeclineRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestDeclined request]) "The request should have been declined"
    }
    
    test "A pending cancellation request is declined" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs Manager
      |> When (DeclineRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationDeclined request]) "The cancellation request should have been declined"
    }
    
  ]
  
[<Tests>]
let cancelTests =
  testList "Cancellation tests" [
    test "A pending request is cancelled by employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A pending request can't be cancelled by another employee" {
      let otherUser = "mbob"
   
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee otherUser)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request shouldn't have been cancelled"
    }
    
    test "A pending request is cancelled by employee before starting date (J-1)" {
   
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A pending request is cancelled by employee at the starting date (J)" {
      let today = Logic.getCurrentDate
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = today; HalfDay = AM }
        End = { Date = today; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationCreated request]) "The request cancellation should have been created"
    }
    
    test "A pending request is cancelled by employee after the starting date (J+1)" {
      let yesterday = Logic.getCurrentDate.Add (TimeSpan.FromDays -1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = yesterday; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationCreated request]) "The request cancellation should have been created"
    }
        
    test "A validated request is cancelled by employee at the starting date (J)" {
      let today = Logic.getCurrentDate
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = today; HalfDay = AM }
        End = { Date = today; HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationCreated request]) "The request cancellation should have been created"
    }
    
    test "A validated request is cancelled by employee after the starting date (J+1)" {
      let yesterday = Logic.getCurrentDate.Add (TimeSpan.FromDays -1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = yesterday; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationCreated request]) "The request cancellation should have been created"
    }
    
    test "A validated request is cancelled by employee before tomorrow (J-1)" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationCreated request]) "The request cancellation should have been created"
    }
   
    test "A validated request is cancelled by manager" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A pending cancellation request is cancelled by manager" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A pending request is cancelled by manager" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A declined cancellation request is cancelled by manager" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestCancellationDeclined request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }

  ]

[<Tests>]
let permissionsTests =
  testList "permissions tests" [
    test "A pending request is declined by employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (DeclineRequest ("jdoe", request.RequestId))
      |> Then (Error "Unsupported request as user") "The decline request can only be done by a Manager"
    }
    
    test "A pending cancellation request is declined by employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (DeclineRequest ("jdoe", request.RequestId))
      |> Then (Error "Unsupported request as user") "The declined request can only be done by a Manager"
    }
    
    test "A pending cancellation request is cancelled by employee" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Unsupported request as user") "The cancel request of a pending cancellation request one can only be done by a Manager"
    }
    
    test "A declined cancellation request is cancelled by employee" {
      let tomorrow = Logic.getCurrentDate.Add (TimeSpan.FromDays 1.)
      let sevenDaysLater = Logic.getCurrentDate.Add (TimeSpan.FromDays 7.)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM } }

      Given [ RequestCancellationDeclined request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Unsupported request as user") "The cancel request of a declined cancellation request one can only be done by a Manager"
    }
    
  ]