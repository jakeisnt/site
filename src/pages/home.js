import { header } from '../html';

const makeHomePage = ({ rootUrl, sourceDir }) => {
  return [
    "body",
    ["html",
     header(),
     ["body",
     // (components/component "sidebar" {:target-path "/index.html"} nil nil [[:h1 "~"]])
      component("Sidebar", { path: `~`, title: 'hi', rootUrl, sourceDir }),
      ["div",
       { class: 'site-body' },
       ["main",
        ["article",
         { class: 'wikipage aboutMe' },
         ["p", "Hey, I'm Jake Chvatal."],
         ["p", "I'm a software engineer based in Stockholm, Sweden."],
         ["p",
          "During the day, I work at "
          ["a", { class: 'external', href: "https://improvin.com" },
           "Improvin'"],

          ", building tools to help food companies reduce their environmental impact."],
         ["p",
          "On nights and weekends, I ",
          ["a", {class: "internal",  href: "/pages/index.html"}, "write"],
         ", take ",
          ["a", {class: "external",  href: "https://instagram.com/jakeisnt"}, "photos"],
         ", and design simple hardware and software tools."],
         component("LastFM")]]]]]];
}

export { makeHomePage };
