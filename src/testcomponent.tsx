import * as ComponentGraph from "./componentgraph";
import { AxiomS, ConfS, TheoremS, AxiomStr, TheoremStr } from "./Scratch.mjs";
import ReactDOM from "react-dom/client";
import React from "react";

type Component = ComponentGraph.Component;

//A bridge between any module that implements the COMPONENT signature from ReScript-land
//and a ComponentGraph component.
function HolComp(RComp: any) {
  return class implements Component {
    dependencyChanged: (id: string, comp: Component) => void;
    deps: Record<string, Component>;
    exports: Record<string, any>;
    loaded: boolean;
    root: ReactDOM.Root;
    stringRep: string;
    toString() {
      return this.stringRep;
    }
    gatherImports() {
      let ret = RComp.Ports.empty;
      for (const x in this.deps) {
        if ("exports" in this.deps[x]) {
          ret = RComp.Ports.combine(ret, this.deps[x]["exports"]);
        }
      }
      console.log("EXPORTS", ret);
      return ret;
    }
    render(signal: (msg: any) => void, loaded: (msg: any) => void) {
      let Tag = RComp.make;
      this.root.render(
        <Tag
          content={this.stringRep}
          imports={this.gatherImports()}
          onChange={(str, exports) => {
            this.exports = exports;
            this.stringRep = str;
            signal(null);
            this.render(signal, loaded);
          }}
          onLoad={(exports, str) => {
            this.exports = exports;
            if (str) {
              this.stringRep = str;
            }
            if (!this.loaded) {
              this.loaded = true;
              loaded(null);
            } else {
              signal(null);
            }
          }}
        />,
      );
    }
    constructor(
      str: string,
      deps: Record<string, Component>,
      signal: (msg: any) => void,
      loaded: (msg: any) => void,
      view?: HTMLElement,
    ) {
      this.deps = deps;
      this.stringRep = str;
      this.loaded = false;
      if (view != null) {
        const newDiv = document.createElement("div");
        view.innerHTML = "";
        view.appendChild(newDiv);
        this.root = ReactDOM.createRoot(newDiv);
      }
      this.render(signal, loaded);
      this.dependencyChanged = (_depName, _comp) => {
        this.render(signal, loaded);
      };
    }
  };
}

window.localStorage.clear();
ComponentGraph.setup({
  "hol-comp": HolComp(AxiomS),
  "hol-config": HolComp(ConfS),
  "hol-proof": HolComp(TheoremS),
  "hol-string": HolComp(AxiomStr),
  "hol-string-proof": HolComp(TheoremStr),
}); //"hol-config": ConfigComponent, "hol-proof":ProofComponent});
