/* OLD SCHOOL CURRENT PLAYING STUFF */
/* source: https://gist.github.com/trisweb/2c0c94273f653c81f34dbe8e85ad30e7 via https://www.trisweb.com/ */
/*
  /other cool idea: changing color and contrast of tbe website background when pressing a key, tapping or clicking
*/

var LFM_API = "https://ws.audioscrobbler.com/2.0/";
var LFM_KEY = "14eb0c0c914456103f2c584d930a44ba"; // Get one at https://secure.last.fm/login?next=/api/account/create
var LFM_USER = "jakeisnt";
var recentTracksUrl =
    LFM_API+"?method=user.getrecenttracks&user="+LFM_USER+"&api_key="+LFM_KEY+"+&format=json&limit=1";

const LFM_TIMEOUT = 1000 * 60; // 1 minute

const lastfm = () => {
  var nowPlayingNode = null;

  function getNowPlaying() {
    get(recentTracksUrl, (response) => {
      var currentTrack = response.recenttracks.track[0];

      // Check if it's the same, if not then rerender
      if (!window.nowPlaying || window.nowPlaying.mbid != currentTrack.mbid) {
        window.nowPlaying = currentTrack;
        renderNowPlaying(currentTrack);
      }
      setTimeout(getNowPlaying, LFM_TIMEOUT);
    });
  }

  function getMetadata(track, currently) {
    return (currently ?
            "<span class=\"np-date\">Playing</span>" :
            "<span class=\"np-date\">Played on "+track.date["#text"]+"</span>") +
      "<br>" +
      "<span class=\"np-title\"><strong>" + track.name + "</strong></span>" +
      "<br/>" +
      "<span class=\"np-artist\">"+track.artist["#text"]+"</span>";
  }

  function renderNowPlaying(track) {
    console.log(track);
    if (nowPlayingNode) {
      nowPlayingNode.remove();
    }

    nowPlayingNode = create('a', {
      className: 'now-playing'
      href: track.url,
      target: 'blank',
    });

    var nowPlayingImage = create('img', {
      className: 'np-image',
      src: track.image.slice(-1)[0]["#text"],
    }, nowPlayingNode);

    var currently = track["@attr"] && track["@attr"].nowplaying == "true";

    var metadata = create('div', {
      class: 'np-metadata',
      innerHTML: getMetadata(track, currently),
    }, nowPlayingNode);

    $(".lastfm-now-playing-box").appendChild(nowPlayingNode);

    setTimeout(() => {
      nowPlayingNode.setAttribute("class", "now-playing loaded");
    }, 100);
  }

  getNowPlaying();
};

lastfm();
