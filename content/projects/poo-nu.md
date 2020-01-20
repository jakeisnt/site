+++
title = "Poo-NU"
date = 2020-01-17T20:59:00-05:00
draft = false
+++

Started at HackHarvard with a team of three other Northeastern students and
myself, Poo-NU is an application to help Northeastern students locate the
closest available restroom. The application can be found at <https://github.com/jakechvatal/poo-nu>.

The application is built with a React Native front-end with Material UI
components and Bootstrap formatting. It interfaces with a GraphQL backend layer
that makes queries to a MySQL databases. We selected these technologies
primarily because we each wanted to learn how to use one of the technologies we
chose; I had no experience with GraphQL and very little with React Native, so I
learned how to apply my React knowledge to a cross-platform application and to
adjust to a new form of querying and API definiton.

Users are currently able to locate the nearest available bathroom via Google
Maps, view which bathrooms are closest to them geographically and key
information about them, such as their average rating and building location. Upon
tapping on a bathroom, users are presented with a more detailed view of the
bathroom with all of its ratings left by other users, and are able to add to
these ratings themselves. We felt that making PooNU not just a utility, but also
a community of others would help Northeastern students bond over their favorite
campus bathrooms.
