const build = (websiteSpec) => {
  // stupid strategy: pretend we are a server.
  // 1. Get the root file's HTML.
  // 2. Find all of the URLs on the file.
  // 3. Convert those URLs into files.
  // 4. Repeat until we have all of the files.
  //
  // Also, reading lots of files / mostly the commits
  // is slow, so make sure to cache everythign.
  //
  // Alternatively, we rewrite 'serve'
  // to collect all of the dependencies at the toplevel,
  // then use those to build the site, and so forth...
  //
  // That seems like the more scalable solution:
  // it'll save tons of fiddling with string parsing lol.
  // schema:
  // serve({ args }) => { result, mimeType, dependencies },
  // then at the end we have all of the dependencies!
}

export { build };
