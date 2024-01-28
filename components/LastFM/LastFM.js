const LastFM = () => ({
  dependsOn: [{ src: '/components/LastFM/lastfm.js' }, { src: '/components/LastFM/lastfm.css' }],
  body: ["div", { class: 'lastfm-now-playing-box' }]
})

export default LastFM;
