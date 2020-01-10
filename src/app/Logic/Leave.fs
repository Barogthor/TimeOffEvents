namespace TimeOff

open System


module Leave =
    let LEGAL_YEAR = DateTime(2019, 1, 1)
    let PER_MONTH_LEAVE = 25. / 12.
    
    let holidays = [
        DateTime(2019, 1, 1);
        DateTime(2019, 4, 13);
        DateTime(2019, 5, 1);
        DateTime(2019, 5, 8);
        DateTime(2019, 5, 21);
        DateTime(2019, 6, 1);
        DateTime(2019, 7, 14);
        DateTime(2019, 8, 15);
        DateTime(2019, 11, 1);
        DateTime(2019, 11, 11);
        DateTime(2019, 12, 25)
    ]

    let getPreviousYear (date: DateTime) =
        (date.AddYears -1).Year
    
    let isNotWEDay (date: DateTime): bool =
        date.DayOfWeek <> DayOfWeek.Saturday && date.DayOfWeek <> DayOfWeek.Sunday
        
    let isHoliday (date: DateTime): bool =
        let tmp = DateTime(2019, date.Month, date.Day)
        let compareDate (holiday: DateTime): bool =
            tmp <> holiday
        holidays |> List.forall compareDate
    
    let listWorkingDaysFromRange (s: DateTime) (e: DateTime)=
        let rec listingDays (date: DateTime) (acc: DateTime list)=
            if date <= e then
                listingDays (date.Add (TimeSpan.FromDays (float 1)) ) (acc @ [date])
            else
                acc
        
        listingDays s []
        |> List.filter isNotWEDay
        |> List.filter isHoliday

    let getAccruedLeave (currentDate: DateTime) =
        float(currentDate.Month - 1) * PER_MONTH_LEAVE
        
    let getTakenLeaveToDate (currentDate: DateTime) (leaves: TimeOffRequest list)=
        let countWorkingDays (timeoff: TimeOffRequest) =
            let mutable count =  0.
            let countTakenLeaves =
                if currentDate >= timeoff.Start.Date && currentDate <= timeoff.End.Date then
                    (float) (listWorkingDaysFromRange timeoff.Start.Date currentDate).Length
                elif currentDate > timeoff.End.Date then
                    (float) (listWorkingDaysFromRange timeoff.Start.Date timeoff.End.Date).Length
                else
                    0.
            count <- count + countTakenLeaves
            if(timeoff.Start.HalfDay = HalfDay.PM) then
                count <- count - 0.5
            if(timeoff.End.HalfDay = HalfDay.AM) then
                count <- count - 0.5
            count
        leaves |> List.sumBy countWorkingDays
    
    let getPlannedLeave (currentDate: DateTime) (leaves: TimeOffRequest list) =
        let countWorkingDays (timeoff: TimeOffRequest) =
            let mutable count =  0.
            let countPlannedForTimeoff =
                if currentDate >= timeoff.Start.Date && currentDate <= timeoff.End.Date then
                    (float) (listWorkingDaysFromRange (currentDate.AddDays 1.) timeoff.End.Date).Length
                elif currentDate < timeoff.Start.Date then
                    (float) (listWorkingDaysFromRange timeoff.Start.Date timeoff.End.Date).Length
                else
                    0.
            count <- count + countPlannedForTimeoff
            if(timeoff.Start.HalfDay = HalfDay.PM) then
                count <- count - 0.5
            if(timeoff.End.HalfDay = HalfDay.AM) then
                count <- count - 0.5
            count
        leaves |> List.sumBy countWorkingDays
        
    let getCarriedOverBalance (currentDate: DateTime) (leaves: TimeOffRequest list) =
        let takenLeaves = getTakenLeaveToDate currentDate leaves
        25. - takenLeaves
    
    let getBalance (currentDate: DateTime) (leaves: TimeOffRequest list) =
        let listCurrentYear = leaves |> List.filter (fun timeoff -> timeoff.Start.Date.Year = currentDate.Year)
        let listPreviousYear = leaves |> List.filter (fun timeoff -> timeoff.Start.Date.Year = currentDate.Year-1)
        let accruedLeaves = getAccruedLeave currentDate
        let plannedLeaves = getPlannedLeave currentDate listCurrentYear
        let takenLeaves = getTakenLeaveToDate currentDate listCurrentYear
        let carriedOverPreviousYear = getCarriedOverBalance currentDate listPreviousYear
        accruedLeaves - plannedLeaves - takenLeaves + carriedOverPreviousYear
        
