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
    let result = Logic.decide userRequestsState user command (Logic.getCurrentDate())
//    System.Console.WriteLine "==============="
//    System.Console.WriteLine globalState
//    System.Console.WriteLine userRequestsState
    Expect.equal result expected message

let getDateFromToday (n: int) =
  Logic.getCurrentDate().Add (TimeSpan.FromDays (float n) )

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let tomorrow = getDateFromToday 1
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }
    
    test "A requests same day overlap" {
      let tomorrow = getDateFromToday 1
      let twoDaysLater = getDateFromToday 2
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = twoDaysLater; HalfDay = AM }
        End = { Date = twoDaysLater; HalfDay = PM }
      }
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = twoDaysLater; HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The 2 requests should overlap for the same day"
    }
    
    test "A requests same day which doesn't overlap" {
      let tomorrow = getDateFromToday 1
      let twoDaysLater = getDateFromToday 2
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM }
      }
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = PM }
        End = { Date = twoDaysLater; HalfDay = AM }
      }
      Expect.isFalse (Logic.overlapsWith request1 request2) "The 2 requests shouldn't overlap"
    }
    
    test "requests shouldn't overlaps" {
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM }
      }
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = sevenDaysLater; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The 2 requests shouldn't overlap"
    }

    test "Requests on 2 distinct days don't overlap" {
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = sevenDaysLater; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    
    test "Requests on 3 distinct days don't overlap" {
      let tomorrow = getDateFromToday 1
      let fourDaysLater = getDateFromToday 4
      let sevenDaysLater = getDateFromToday 7
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM }
      }

      let otherRequests = List.toSeq [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = fourDaysLater; HalfDay = AM }
          End = { Date = fourDaysLater; HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = sevenDaysLater; HalfDay = AM }
          End = { Date = sevenDaysLater; HalfDay = PM }
        }
      ]

      Expect.isFalse (Logic.overlapsWithAnyRequest otherRequests request) "The requests don't overlap"
    }
    
    test "Requests overlaps on a range" {
      let tomorrow = getDateFromToday 1
      let fourDaysLater = getDateFromToday 4
      let sevenDaysLater = getDateFromToday 7
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = sevenDaysLater; HalfDay = PM }
      }

      let otherRequests = List.toSeq [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = fourDaysLater; HalfDay = AM }
          End = { Date = sevenDaysLater; HalfDay = PM }
        };
      ]

      Expect.isTrue (Logic.overlapsWithAnyRequest otherRequests request) "The requests range overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let tomorrow = getDateFromToday 1
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
    
    test "A request isn't overlapping" {
      let tomorrow = getDateFromToday 1
      let twoDaysLater = getDateFromToday 2
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = PM }
        End = { Date = twoDaysLater; HalfDay = PM } }

      Given [ RequestCreated request1 ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request2)
      |> Then (Ok [RequestCreated request2]) "The request should have been created"
    }
    
    test "A request is overlapping" {
      let tomorrow = getDateFromToday 1
      let twoDaysLater = getDateFromToday 7
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = twoDaysLater; HalfDay = PM } }

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
      let tomorrow = getDateFromToday 1
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

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
      let tomorrow = getDateFromToday 1
      
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (DeclineRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestDeclined request]) "The request should have been declined"
    }
    
    test "A pending cancellation request is declined" {
      let tomorrow = getDateFromToday 1
      
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

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
      let tomorrow = getDateFromToday 1
      
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A pending request can't be cancelled by another employee" {
      let otherUser = "mbob"
      let tomorrow = getDateFromToday 1
   
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee otherUser)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request shouldn't have been cancelled"
    }
    
    test "A pending request is cancelled by employee before starting date (J-1)" {
      let tomorrow = getDateFromToday 1
   
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee request.UserId)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    
    test "A pending request is cancelled by employee at the starting date (J)" {
      let today = Logic.getCurrentDate()
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
      let yesterday = getDateFromToday -1
      let sevenDaysLater = getDateFromToday 7
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
      let today = Logic.getCurrentDate()
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
      let yesterday = getDateFromToday -1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = tomorrow; HalfDay = AM }
        End = { Date = tomorrow; HalfDay = PM } }

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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
      let tomorrow = getDateFromToday 1
      let sevenDaysLater = getDateFromToday 7
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
  
[<Tests>]
let leaveTests =
  testList "leave balance tests" [
    test "range should have only working days" {
      let sylvestre = DateTime.Parse("31/12/2019")
      let tenDaysLater = DateTime.Parse("09/01/2020")
      let daysFromTomorrowToTenDaysLater = Leave.listWorkingDaysFromRange sylvestre tenDaysLater
      let countWorkingDays = daysFromTomorrowToTenDaysLater.Length
      let devoidOfWEDays = daysFromTomorrowToTenDaysLater |> List.forall Leave.isNotWEDay
      Expect.isTrue devoidOfWEDays "the range has WE and holiday days that should have been filtered"
      Expect.equal countWorkingDays 7 "without WE days, the count is 8"
    }
    
    test "" {
      let timeoff1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime.Parse("20/12/2019"); HalfDay = AM }
        End = { Date = DateTime.Parse("25/12/2019"); HalfDay = PM } }
      let timeoff2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime.Parse("26/12/2019"); HalfDay = AM }
        End = { Date = DateTime.Parse("28/12/2019"); HalfDay = PM } }
      let timeoff3 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime.Parse("29/12/2019"); HalfDay = AM }
        End = { Date = DateTime.Parse("31/12/2019"); HalfDay = PM } }
      let timeoff4 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime.Parse("29/12/2018"); HalfDay = AM }
        End = { Date = DateTime.Parse("31/12/2018"); HalfDay = PM } }
      let list = [timeoff1; timeoff2; timeoff3; timeoff4]
      let year2019 = DateTime(2019,12,30)
      let accruedLeaves = int(Leave.getAccruedLeave year2019)
      let takenLeaves = Leave.getTakenLeaveToDate year2019 (list |> List.filter (fun timeoff -> timeoff.Start.Date.Year = year2019.Year))
      let plannedLeaves = Leave.getPlannedLeave year2019 (list |> List.filter (fun timeoff -> timeoff.Start.Date.Year = year2019.Year))
      let leavesBalance = int(Leave.getBalance year2019 list)
      Expect.equal accruedLeaves 22 "A whole year minus 1 month  should give 22 days off"
      Expect.floatClose Accuracy.low takenLeaves 6. "The total timeoff leaves taken should be 6"
      Expect.floatClose Accuracy.low plannedLeaves 1. "The total timeoff planned should be 1"
      Expect.equal leavesBalance 39 "There should be 39 days off left after the balance"
      
    }
  ]