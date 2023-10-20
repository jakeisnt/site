const sourceUrl = 'https://github.com/jakeisnt/wiki';
const siteName = "Jake Chvatal";

const sitePaths = [
  { folder: "resources", forceRebuild: true },
  { folder: "components" },
  { folder: "src", forceRebuild: true },
  { folder: "style", forceRebuild: true }
];

const wikiPaths = [
  {
    folder: "pages",
    sortBy: (v) => v.lastLog.commitDate,
  },
  {
    folder: "scripts",
    sortBy: (v) => v.lastLog.commitDate,
  },
  {
    folder: "journals",
    sortBy: "name",
  }
];

const sources = [
  { dir: "/home/jake/site", paths: sitePaths },
  { dir: "/home/jake/wiki", paths: wikiPaths }
];

const website = {
  sources,
  target: "/home/jake/site/docs"
};

const localPort = 4242;

const profiles = [
  {
    name: "Are.na",
    url: "https://are.na/jake-isnt",
    user: "jake-isnt"
  },
  {
    name: "Mastodon",
    url: "https://merveilles.town/@jakeisnt",
    user: "jakeisnt"
  },
  {
    name: "Twitter",
    url: "https://twitter.com/jakeissnt",
    user: "jakeissnt"
  },
  // ... (other profile objects)
];

const personal = {
  name: "Jake Chvatal",
  location: "Stockholm, SV",
  occupation: "Software Engineer",
  pronouns: ["he", "him", "his"],
  education: {
    degree: "B.S. Computer Science",
    school: "Northeastern University",
    url: "https://www.khoury.northeastern.edu/"
  }
};

const lastModifiedFile = "docs/last-modified.txt";
