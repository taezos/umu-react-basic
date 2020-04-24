import React from "react";
import ReactDOM from "react-dom";

import Main from "../output/Main";

function main () {
  const component = React.createElement(Main.mainJSX, { label: "MainComponent" });

  ReactDOM.render(component, document.getElementById("app"));
}

if ( module.hot ) {
  module.hot.accept(function () {
    console.log("[INFO]: Running main...");
    main();
  })
}

console.log("[INFO]: Starting...");
main();
