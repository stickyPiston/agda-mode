// @ts-nocheck
import Main from "./jAgda.Extension.mjs";
import * as vscode from "vscode";
export function activate(context) { Main.activate(vscode)(context)(() => { console.log("Finished execution"); }); }
export function deactivate() { }