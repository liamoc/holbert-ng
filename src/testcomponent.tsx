import * as ComponentGraph from './componentgraph'
import { AxiomS, ConfS, TheoremS, InductiveS } from './Scratch.mjs'
import ReactDOM from 'react-dom/client';
import React from 'react';




type Component = ComponentGraph.Component 

//A bridge between any module that implements the COMPONENT signature from ReScript-land
//and a ComponentGraph component.
function HolComp(RComp : any) {
	return class implements Component {
		dependencyChanged : (id: string, comp: Component) => void;
		deps : Record<string,Component>;
		exports : Record<string,any>;
		loaded: boolean;
		root : ReactDOM.Root;
		state: any;
		toString() {
			return RComp.serialise(this.state)
		}
		gatherImports() {
			let ret = RComp.Ports.empty
			for (const x in this.deps) {
				if ("exports" in this.deps[x]) {
					ret = RComp.Ports.combine(ret,this.deps[x]["exports"])
				}
			}
			return ret
		}
		render(signal : (msg: any) => void) {
			let Tag = RComp.make			
			this.root.render(
				<Tag content={this.state} imports={this.gatherImports()} 
					onChange={ (state, exports) => {
						if (exports) {
							this.exports = exports
							signal(null)							
						}
						this.state = state;
						this.render(signal)
					}} 				
				/>
			)
		}
		constructor(str: string,deps : Record<string,Component>, signal : (msg: any) => void, loaded : (msg: any) => void, view? : HTMLElement) {
			this.deps = deps
			this.loaded = false
			if (view != null) { 
				const newDiv = document.createElement("div");	
				view.innerHTML = "";
				view.appendChild(newDiv);
				this.root = ReactDOM.createRoot(newDiv); 
			}
			const imports = this.gatherImports();
			var foo = RComp.deserialise(str,imports)			
			if (foo.TAG == "Ok") {
				this.exports = foo._0[1]
				this.state = foo._0[0]
				this.dependencyChanged = (_depName, _comp) => {
					this.render(signal)
				};
				loaded(null)
				this.loaded = true;
				this.render(signal)
			} else {
				view.innerHTML = foo._0
			}
		}
	}
	
}

window.localStorage.clear()
ComponentGraph.setup({
	"hol-comp": HolComp(AxiomS),
	"hol-inductive": HolComp(InductiveS),
	"hol-config":HolComp(ConfS),
	"hol-proof": HolComp(TheoremS)
});
