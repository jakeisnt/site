+++
title = "CDK Global Internship"
draft = false
+++

At CDK Global, I leveraged React.js and Redux to, along with a high school
intern and two college interns, develop software used by an internal credit
reporting team to monitor the number of credit checks made by individual car
dealerships.


## Tech Stack {#tech-stack}

We built a full-stack web application with a React.js front-end, making use of
both Material UI and CDK Global's proprietary React components as well as Redux
and thunk for asynchronous global state management. Interesting challenges posed
by this project include designing the inheritance hierarchy through which React
components were displayed, as the desire of the end user to display a
spreadsheet that presented statistical information in different forms in
different modes, and designing an efficient way to manage global state with
Redux to make a minimal number of request while keeping the information
displayed up to date. The former problem was resolved by Redux to connect
disparate components to their corresponding information without making many
additional requests, and the latter through a custom Redux store method to
refresh the data asynchronously as the application is used, refreshed and as
information with different priorities was desired.

The backend was constructed with the Spring Boot Java framework; the entire
application was run on an Nginx server running in a Docker container on CDK's
servers. To this end, I helped format the server and docker container to run the
front-end on CDK's development, testing and production servers, and communicated
with the Spring Boot developers to ensure the data representations presented by
our API were best conducive to the needs of the end user on the front-end (as
structure informs representation).


## Agile Organization {#agile-organization}

Our team made use of a weekly Agile/Scrum organizational paradigm, in which we
held sprint planning sessions to craft workloads, daily stand-ups to update and
review progress, as well as reflection sessions to ensure that the system and
each of its participants was running smoothly. We made heavy use of Atlassian's
Jira organizational framework to organize this procedure, and for compatibility
we used Stash as our git repository of choice as well as various other Atlassian
services for the purposes of communication and documentation. As typical with
Agile, development workflow started with a weekly assignment of stories,
then creating a feature branch for each story to be completed and providing
detailed instructions by which other team members could validate the work once
the work was completed (as three other team members were required to approve a
commit before it could be staged and merged into our master branch).

Also of note was the extensive communication with our product manager - the
objective of our project was not very well-defined when the internship began, so
we set up extensive meetings with the non-technical end user, preparing
questions and creating user stories for various desires of the end user based on
responses and feedback on the current status of the application. Eventually, we
found that receiving some answers to basic desires of functionality, then
crafting user stories based on this feedback and presenting the end user with
various iterations of demo applications to determine what the user desired, was
most effective in determining concrete end goals for the project.
