// Learn more about F# at http://fsharp.org
open System
open System.IO
open System.Text.RegularExpressions
(*
type MetaData = {title: string;
                 description: string;
                 keywords: string;
                 author: string;
                 manager: string;
                 date: DateTime;
                 topic: string;
                 prod: string;
                 technology: string;
                 assetid: Guid} *)

let docFolder = @"C:\Users\Owner\Documents\FSharp\visualfsharpdocs\docs\conceptual"

let listBaseFile docFolder functionName =
    let files = Directory.GetFiles(docFolder)
    files 
    |> Array.filter (fun i -> i.Contains(functionName))
    |> Array.map (fun i -> i.Replace(docFolder, ""))

let pathSeparationCharacters = [|'/'; '\\'|]

let cleanStart (fileName: string) = 
    fileName.TrimStart(pathSeparationCharacters)

let convertFileEnumerationToQuestion (files: string []) =
    match files with 
    | [||] -> sprintf "No base file"
    | _ -> let start = [|sprintf "Select one of the following files: "|]
           let cleanFileNames = files 
                                |> Array.mapi (fun i value-> 
                                                   let clean = cleanStart value
                                                   sprintf "    %d:    %s" (i + 1) clean)
           let concat = Array.append start cleanFileNames
           String.concat "\n" concat

let readUserFileSelection (files: string []) =
    let selection = Console.ReadLine()
    match Int32.TryParse(selection) with
    | true, index -> match index with
                     | i when i > 0 && i < (files.Length + 1) -> files.[i-1]
                     | _ -> failwith "Index out of bounds"
    | _ -> failwith "Non-integer input"

let findBaseModuleName (baseFile:string) = 
    let cleanedName = cleanStart baseFile
    cleanedName.Split('.').[0]

let removeDoubleEmptyLines alist =
    let rec remove alist prevElement acc =
        match alist with
        | [] -> acc
        | h::t -> if h = "" && prevElement = "" then
                      remove t "" acc
                  else
                      remove t h (h::acc)
    remove alist "" []
    |> List.rev

let cleanFile fileName = 
    let lines = File.ReadAllLines(fileName)
    let newLines = lines
                   |> Array.toList
                   |> removeDoubleEmptyLines
                   |> List.toArray
    File.WriteAllLines(fileName, newLines)

let makeNewFile docFolder (baseFile: string) (baseModuleName: string) newModuleName = 
    let split = baseFile.Split('.')
    split.[0] <- (split.[0]).Replace(baseModuleName, newModuleName)
    let newFileName = String.concat "." split 
    try
        let baseFileName = docFolder + baseFile
        cleanFile baseFileName
        let newFileName = docFolder + newFileName
        File.Delete(newFileName)
        File.Copy(baseFileName, newFileName)
        newFileName
    with
        | _ -> failwith "File Copy Failed"

let capitalize (astring: string) =
    let charArray = astring.ToLower().ToCharArray()
    match charArray with 
    | [||] -> ""
    | _ -> charArray.[0] <- charArray.[0].ToString().ToUpper().Chars(0)
           new String(charArray)

let findAndModifySingleLine (lines: string []) lineInfo newPropertyValue (stringModify: string option) = 
    let line = lines
                   |> Array.tryFindIndex (fun i -> i.StartsWith(lineInfo))
    match line with
    | None -> printfn "Could not find singleLine %A" lineInfo
    | Some index -> 
        match stringModify with
        | Some oldModuleName -> 
            lines.[index] <- lines.[index].Replace(capitalize oldModuleName, capitalize newPropertyValue)
        | None -> 
            lines.[index] <- lineInfo + ": " + newPropertyValue

let moduleType moduleName =
    match moduleName with 
    | "seq" -> "seq<'T>"
    | "list" -> "'T list"
    | "array" -> "'T []"
    | _ -> failwith "Not one of the supported modules" 

let modifyLine (line: string) conversionFunction baseModuleName newModuleName =
    let oldModuleType = conversionFunction baseModuleName
    if line.Contains(oldModuleType) then
        let newModuleType = conversionFunction newModuleName
        line.Replace(oldModuleType, newModuleType)
    else
        line

let modifyModuleType (i: string) baseModuleName newModuleName = 
    modifyLine i moduleType baseModuleName newModuleName

let safeFunctionName moduleName = 
    (capitalize moduleName) + "."

let modifyFunctionCall (i: string) baseModuleName newModuleName = 
    modifyLine i safeFunctionName baseModuleName newModuleName

let getTypeSignature moduleName =
    match moduleName with 
    | "seq" -> "Type: [seq](https://msdn.microsoft.com/library/2f0c87c6-8a0d-4d33-92a6-10d1d037ce75)**&lt;'T&gt;**"
    | "list" -> "Type: **'T**[list](https://msdn.microsoft.com/library/c627b668-477b-4409-91ed-06d7f1b3e4a7)"
    | "array" -> "Type: **'T [[]](https://msdn.microsoft.com/library/def20292-9aae-4596-9275-b94e594f8493)**"
    | _ -> failwith "Not one of the supported modules" 

let modifyTypeSignature (i: string) baseModuleName newModuleName =
    modifyLine i getTypeSignature baseModuleName newModuleName

let moduleFullName moduleName = 
    match moduleName with 
    | "seq" -> "sequence"
    | "list" -> "list"
    | "array" -> "array"
    | _ -> failwith "Not one of the supported modules" 

let generateNewSnippetNumber fileNameBase docFolder =
    let rec generate fileNameBase docFolder numList =
        match numList with
        | [] -> failwith "No valid new snippet filename"
        | h::t -> let newFileName = fileNameBase + h.ToString() + ".fs"
                  if File.Exists(docFolder + "\\" + newFileName) then
                      generate fileNameBase docFolder t
                  else
                      newFileName
    generate fileNameBase docFolder [1..10000]

let modifySnippet (lines: string []) newModuleName baseModuleName docFolder =
    let start = "[!code-fsharp[Main]("
    let lineIndex = 
        lines
        |> Array.tryFindIndex (fun i -> i.StartsWith(start))
    match lineIndex with
    | None -> printfn "Could not find snippet line"
    | Some index -> let line = lines.[index]
                    let path = line.Substring(start.Length).Split(')').[0]
                    let mutable newPath = path.Replace(moduleFullName baseModuleName, moduleFullName newModuleName)
                    try
                        File.Copy(docFolder + "\\" + path, docFolder + "\\" + newPath)
                    with
                        | :? IOException ->
                            let fileName = newPath.Split('.').[0]
                            let finalIndex = fileName.LastIndexOf("t") + 1 //snippet ends with t!
                            let snippetName = fileName.Remove(finalIndex)
                            let newFileName = generateNewSnippetNumber snippetName docFolder
                            newPath <- newFileName
                            File.Copy(docFolder + "\\" + path, docFolder + "\\" + newPath)
                    let newLine = newPath
                    lines.[index] <- start + newLine + ")]"


let modifyNewFile newFile baseModuleName newModuleName docFolder author =
    let lines = File.ReadAllLines(newFile)
    let metaEndIndex = lines |> Array.find (fun i -> i = "---")
    findAndModifySingleLine lines "title" newModuleName (Some(baseModuleName))
    findAndModifySingleLine lines "description" newModuleName (Some(baseModuleName))
    findAndModifySingleLine lines "author" author None
    findAndModifySingleLine lines "ms.date" (DateTime.Now.ToString("d")) None
    let newGuid = Guid.NewGuid().ToString()
    findAndModifySingleLine lines "ms.assetid" newGuid None
    findAndModifySingleLine lines "**Namespace/Module Path:** Microsoft.FSharp.Collections." newModuleName (Some(baseModuleName))
    modifySnippet lines newModuleName baseModuleName docFolder
    findAndModifySingleLine lines "Supported in: " "" (Some("2.0, "))
    findAndModifySingleLine lines "[Collections." newModuleName (Some(baseModuleName))
    let newLines = 
        lines 
        |> Array.map (fun i -> modifyModuleType i baseModuleName newModuleName)
        |> Array.map (fun i -> modifyFunctionCall i baseModuleName newModuleName)
        |> Array.map (fun i -> modifyTypeSignature i baseModuleName newModuleName)
    File.WriteAllLines(newFile, newLines)

[<EntryPoint>]
let main argv = 
    let files = listBaseFile docFolder "distinct"
    let printstr = convertFileEnumerationToQuestion files
    printfn "%s" printstr
    let baseFile = readUserFileSelection files
    let moduleName = findBaseModuleName baseFile
    let newModuleName = "list"
    let newFile = makeNewFile  docFolder baseFile moduleName newModuleName
    modifyNewFile newFile moduleName newModuleName docFolder "liboz"
    0 // return an integer exit code
