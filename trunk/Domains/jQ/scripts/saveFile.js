// Stolen from tiddlywiki

function javaUrlToFilename(url)
{
    var f = "//localhost";
    if(url.indexOf(f) == 0)
        return url.substring(f.length);
    var i = url.indexOf(":");
    return i > 0 ? url.substring(i-1) : url;
}

function javaSaveFile(filePath,content)
{
    try {
        if(document.applets["TiddlySaver"])
            return document.applets["TiddlySaver"].saveFile(javaUrlToFilename(filePath),"UTF-8",content);
    } catch(ex) {
    }
    try {
        var s = new java.io.PrintStream(new java.io.FileOutputStream(javaUrlToFilename(filePath)));
        s.print(content);
        s.close();
    } catch(ex) {
        return null;
    }
    return true;
}

function javaLoadFile(filePath)
{
    try {
        if(document.applets["TiddlySaver"])
            return String(document.applets["TiddlySaver"].loadFile(javaUrlToFilename(filePath),"UTF-8"));
    } catch(ex) {
    }
    var content = [];
    try {
        var r = new java.io.BufferedReader(new java.io.FileReader(javaUrlToFilename(filePath)));
        var line;
        while((line = r.readLine()) != null)
            content.push(new String(line));
        r.close();
    } catch(ex) {
        return null;
    }
    return content.join("\n");
}

function ieCreatePath(path)
{
    try {
        var fso = new ActiveXObject("Scripting.FileSystemObject");
    } catch(ex) {
        return null;
    }

    var pos = path.lastIndexOf("\\");
    if(pos==-1)
        pos = path.lastIndexOf("/");
    if(pos!=-1)
        path = path.substring(0,pos+1);

    var scan = [path];
    var parent = fso.GetParentFolderName(path);
    while(parent && !fso.FolderExists(parent)) {
        scan.push(parent);
        parent = fso.GetParentFolderName(parent);
    }

    for(i=scan.length-1;i>=0;i--) {
        if(!fso.FolderExists(scan[i])) {
            fso.CreateFolder(scan[i]);
        }
    }
    return true;
}

// Returns null if it can't do it,
// false if there's an error,
// true if it saved OK
function ieSaveFile(filePath,content)
{
    ieCreatePath(filePath);
    try {
        var fso = new ActiveXObject("Scripting.FileSystemObject");
    } catch(ex) {
        return null;
    }
    var file = fso.OpenTextFile(filePath,2,-1,0);
    file.Write(content);
    file.Close();
    return true;
}

// Returns null if it can't do it,
// false if there's an error,
// or a string of the content if successful
function ieLoadFile(filePath)
{
    try {
        var fso = new ActiveXObject("Scripting.FileSystemObject");
        var file = fso.OpenTextFile(filePath,1);
        var content = file.ReadAll();
        file.Close();
    } catch(ex) {
        return null;
    }
    return content;
}

function ieAskFile () {
    try { // XPSP2 IE only
        var s = new ActiveXObject('UserAccounts.CommonDialog');
        s.Filter='All files|*.*|Text files|*.txt|HTML files|*.htm;*.html|XML files|*.xml|';
        s.FilterIndex=defext=='txt'?2:'html'?3:'xml'?4:1;
        s.InitialDir=path;
        s.FileName=file;
        if (s.showOpen()) var result=s.FileName;
    }
    catch(e) {  // fallback
        var result=prompt(msg,path+file);
    }
}

function mozillaAskFile () {
    if(window.Components) { // moz
        try {
            netscape.security.PrivilegeManager.enablePrivilege('UniversalXPConnect');
            var nsIFilePicker = window.Components.interfaces.nsIFilePicker;
            var picker = Components.classes['@mozilla.org/filepicker;1'].createInstance(nsIFilePicker);
            picker.init(window, msg, nsIFilePicker.modeSave);
            var thispath = Components.classes['@mozilla.org/file/local;1'].createInstance(Components.interfaces.nsILocalFile);
            thispath.initWithPath(path);
            picker.displayDirectory=thispath;
            picker.defaultExtension=defext;
            picker.defaultString=file;
            picker.appendFilters(nsIFilePicker.filterAll|nsIFilePicker.filterText|nsIFilePicker.filterHTML);
            if (picker.show()!=nsIFilePicker.returnCancel) var result=picker.file.persistentDescriptor;
        }
        catch(e) { alert('error during local file access: '+e.toString()) }
    }
}

// Returns null if it can't do it,
// false if there's an error,
// true if it saved OK
function mozillaSaveFile(filePath,content)
{
    if(window.Components) {
        try {
            netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");
            var file = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);
            file.initWithPath(filePath);
            if(!file.exists())
                file.create(0,0664);
            var out = Components.classes["@mozilla.org/network/file-output-stream;1"].createInstance(Components.interfaces.nsIFileOutputStream);
            out.init(file,0x20|0x02,00004,null);
            out.write(content,content.length);
            out.flush();
            out.close();
            return true;
        } catch(ex) {
            return false;
        }
    }
    return null;
}

// Returns null if it can't do it,
// false if there's an error,
// or a string of the content if successful
function mozillaLoadFile(filePath)
{
    if(window.Components) {
        try {
            netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");
            var file = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);
            file.initWithPath(filePath);
            if(!file.exists())
                return null;
            var inputStream = Components.classes["@mozilla.org/network/file-input-stream;1"].createInstance(Components.interfaces.nsIFileInputStream);
            inputStream.init(file,0x01,00004,null);
            var sInputStream = Components.classes["@mozilla.org/scriptableinputstream;1"].createInstance(Components.interfaces.nsIScriptableInputStream);
            sInputStream.init(inputStream);
            var contents = sInputStream.read(sInputStream.available());
            sInputStream.close();
            inputStream.close();
            return contents;
        } catch(ex) {
            return false;
        }
    }
    return null;
}

function saveFile(fileUrl,content)
{
    var r = mozillaSaveFile(fileUrl,content);
    if(!r)
        r = ieSaveFile(fileUrl,content);
    if(!r)
        r = javaSaveFile(fileUrl,content);
    return r;
}

function loadFile(fileUrl)
{
    var r = mozillaLoadFile(fileUrl);
    if((r == null) || (r == false))
        r = ieLoadFile(fileUrl);
    if((r == null) || (r == false))
        r = javaLoadFile(fileUrl);
    return r;
}
