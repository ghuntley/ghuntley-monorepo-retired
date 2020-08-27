---
layout: notes
title: user stories
---

# independent

Stories should be self-contained and no inherent dependency on another user
stories.

* Avoid mentioning other user stories in the Acceptance Criteria.
* If you can't avoid dependencies then user stories need to be combined or
  merged.
* Attach all atrefacts required for getting that story done. (eg UI/UX)

> As a user I want to be able to login to system, so that I would be able to
> see dashboard
> 
> Acceptance Criteria – 1 :
> 
>  As a user I can login using my email and password
>
> Acceptance Criteria – 2:
>
>  No other users would be able to see my dashboard
> 
> Note:
>
>  Make sure user cannot login with empty credentials (either usename or password)
>  Make sure the logout works
>  Make sure the menu shows the user profile


# negotiable

Nothing is fixed and set in stone except the iteration on the backlog. Stories
on a backlog or even the entire backlog can be clarified, rewritten or
discarded.

* Team members will and should ask for clarification.
* Be open to feedback from refinement.
* Backlogs might go through a few revisions before they’d be ready for
  execution. Remember they are a living and breathing artifact of Scrum.

If two user stories are covering similar be haviour it makes sense for the team
to neogitate and merge the stories. Likewise if the story is too big to
effectively estimate or complete in a sprint it then the task should cut into
smaller logical parts which will aide with the identificaiton of unknown-unknowns.

# valuable

If it's not providing value to the project stake holders why are you doing it?

All technology spikes should have clear outcomes in the form of scientific theory
detailed before the task begins and should be executed within a timebox.


Breaking down user stories into separate user story for front-end and back-end
does not provide value because it is not releasable to customers


# estimatable

If a user story size cannot be estimated, it should not be planned, tasked or
included into the sprint. If there's a lack of supporting information in the
story that prevents estimation then how did the story end up in the sprint?
It's time to re-review the defintion of ready.

# testable

Can you test the story in isolation? Have you covered both happy and failure
test scenarios? Ensure that the acceptance critera is independant and does not
take a dependancy on other stories otherwise it can't be marked `Done` until
the dependancies ship.
