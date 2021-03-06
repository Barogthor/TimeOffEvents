This is a starter project to use in an F# coding assignment working with event-sourcing and modeling a workflow.

It relies on .NET core and references Expecto as a testing library/runner.

Just clone the repo or download it, and get started coding and testing in F#!

To get started, just dotnet run in the folder to run the Expecto tests.
<h2>Example Timeoff request:</h2>
<pre><code>
{
	"userId": "employee1",
	"requestId": "4c569e0e-fd7a-4f5c-80d2-d7f286691cb3",
	"start": {
		"date": "02/22/2020",
		"halfDay": {"Case": "AM" }
	},
	"end":{
		"date": "02/23/2020",
		"halfDay": {"Case": "PM" }
	}
}
</code></pre>

<h2>Example validate, cancel, decline (json):</h2>
<pre><code>
{
      "userId": "employee1",
      "requestId": "4c569e0e-fd7a-4f5c-80d2-d7f286691cb3"
}
</code></pre>