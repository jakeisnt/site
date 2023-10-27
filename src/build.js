const build = (websiteSpec) => {
  // stupid strategy: pretend we are a server.
  // 1. Get the root file's HTML.
  // 2. Find all of the URLs on the file.
  // 3. Convert those URLs into files.
  // 4. Repeat until we have all of the files.
  //
  // Also, reading lots of files / mostly the commits
  // is slow, so make sure to cache everythign.
}

export { build };
