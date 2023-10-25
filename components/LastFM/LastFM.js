const LastFM = () => ({
  dependsOn: [{ src: './lastfm.js' }, { src: './lastfm.css' }],
  body: ["div", { class: 'lastfm-now-playing-box' }]
})
