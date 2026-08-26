
export interface Component {
	toString(): string;
	dependencyChanged(id: string, comp: Component, msg: any): void;
}

let toInitialise: Array<string> = [];

function fetchAsDocument(url: string): Promise<{ doc: Document; contentType: string }> {
	return new Promise((resolve, reject) => {
		const xhr = new XMLHttpRequest();
		xhr.open("GET", url);
		xhr.responseType = "document";
		xhr.onload = () => {
			if (xhr.responseXML) {
				resolve({ doc: xhr.responseXML, contentType: xhr.getResponseHeader("content-type") || "" });
			} else {
				reject(new Error(`Failed to parse document at ${url}`));
			}
		};
		xhr.onerror = () => reject(new Error(`Network error fetching ${url}`));
		xhr.send();
	});
}

export async function load(url: string): Promise<Handler> {
	if (url in database) {
		console.log(database[url],"URL");
		return database[url];
	} else {
		let requestURI = url.split('/').slice(0, -1).join("/");
		const { doc, contentType } = await fetchAsDocument(requestURI);
		const isXml = /\bxml\b/i.test(contentType) && !/html/i.test(contentType);
		
		let htmldoc: Document;
		
		if (isXml) {
			const { doc: xsltDoc } = await fetchAsDocument("/default.xsl"); // one fetch for the stylesheet, unavoidable — it's a second resource
			const xsltProcessor = new XSLTProcessor();
			xsltProcessor.importStylesheet(xsltDoc);
			htmldoc = xsltProcessor.transformToDocument(doc);
		} else {
			htmldoc = doc;
		}
				
		for (let x of knownTags) {
			for (let y of htmldoc.querySelectorAll(x)) {
				window.customElements.upgrade(document.adoptNode(y));
			}
		}
		while (toInitialise.length) {
			database[toInitialise.shift() ?? ""]?.initialise();
		}
		if (!(url in database)) {
			throw "ERROR"
		} else {
			return database[url];
		}
	}
}

class Handler {
	url: string;
	component: Component | null;
	subscribers: Array<Handler>;
	deps: Record<string, Handler | null>;
	status: "loading" | "ready";

	addSubscriber(h: Handler) {
		this.subscribers.push(h);
		if (this.status == "ready") {
			h.dependencyReady(this.url)
		}
	}
	initialise: () => void;
	dependencyReady: (url: string) => void;
	notifySubscribers: (msg: any) => void;
	constructor(
		url: string,
		textual: string,
		original_str: string,
		deps: Array<string>,
		maker: new (
			data: string,
			deps: Record<string, Component>,
			signal: (msg: any) => void,
			initialised: (msg: any) => void,
			reset: (msg: any) => void,
			view?: HTMLElement) => Component,
		view?: HTMLElement) {
		this.url = url;
		this.status = "loading";
		this.deps = {};
		let awaiting: Array<string> = [];
		this.subscribers = [];
		for (let dep of deps) {
			if (dep != "") {
				awaiting.push(dep);
				this.deps[dep] = null;
			}
		}
		this.component = null;
		this.notifySubscribers = function(msg: any) {
			console.log(this.url, this.subscribers)
			if (this.component != null) {
				for (let sub of this.subscribers) {
					if (sub.component != null) {
						sub.component.dependencyChanged(this.url, this.component, msg)
					}
				}
				window.localStorage.setItem(this.url, this.component.toString())
			}
		}
		
		
		let setupComponent =  (text : string, deps: Record<string,Component>) => {
			this.component = new maker(text, deps,
			(msg) => { this.notifySubscribers(msg) }, (msg) => {
				this.status = "ready";
				for (let sub of this.subscribers) {
					sub.dependencyReady(this.url);
				}
			}, (msg) => {
				setupComponent(original_str, deps);
				this.notifySubscribers(msg);
			}, view);
		}
		
		this.initialise = function() {
			for (let dep in this.deps) {
				load(dep).then((h) => {
					this.deps[dep] = h;
					h.addSubscriber(this);
				})
			}
			if (awaiting.length == 0) {
				setupComponent(textual, {})
			}
		}
		this.dependencyReady = function(url) {
			let index = awaiting.indexOf(url);
			if (index > -1) {
				awaiting.splice(index, 1);
			}

			if (awaiting.length == 0) {
				let deps2: Record<string, Component> = {};
				for (let dep in this.deps) {
					let v = this.deps[dep]?.component;
					if (v != null && v != undefined) {
						deps2[dep] = v;
					}
				}				
				setupComponent(textual, deps2);
			}
		}
	}
}

let knownTags: Array<string> = [];
export let database: Record<string, Handler> = {};

export function setup(
	spec: Record<string,
		new (data: string,
			deps: Record<string, Component>,
			signal: (msg: any) => void,
			initialised: (msg: any) => void,
			reset: (msg: any) => void,
			view?: HTMLElement) => Component>) {
	let promises = [];
	for (const name in spec) {
		const maker = spec[name];
		knownTags.push(name);
		window.customElements.define(name, class extends HTMLElement {
			constructor() {
				super();
				let id = this.attributes.getNamedItem("id")?.value ?? "default";
				toInitialise.push(id);
				let deps = (this.attributes.getNamedItem("deps")?.value ?? "")
					.trim()
					.split(/\s+/)
					.filter(Boolean);
				console.log(deps);			
				let original_str = this.innerHTML;
				let text = window.localStorage.getItem(id) ?? this.innerHTML;
				this.innerHTML = "loading";
				if (this.id in database) {
					this.innerHTML = "duplicate element"
				} else {
					database[this.id] = new Handler(this.id, text, original_str, deps, maker, this);
				}
			}
		});
		promises.push(window.customElements.whenDefined(name))
	}
	Promise.all(promises).then(() => {
		while (toInitialise.length) {
			database[toInitialise.shift() ?? ""]?.initialise();
		}
	})
}