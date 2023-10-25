const Neko = () => ({
  dependsOn: [{ src: './neko.js' }, { src: './neko.css' }],
  body: ["span",
         ["div", { class: "neko-bed" }, ""],
         ["div", { class: "neko" }, ""]],
})

export default Neko;
