SmalltalkMTExporter defaultPath: 'd:\smalltalk\philemon-typewise-vast'.

(SmalltalkMTExporter for: PhilemonKernelExtensions) exportToFile.
(SmalltalkMTExporter for: PhilemonTerminalCommon) exportToFile.
(SmalltalkMTExporter for: PhilemonTerminalClient) exportToFile.
(SmalltalkMTExporter for: PhilemonTerminalView) exportToFile.

SmalltalkMTExporter defaultPath: 'd:\smalltalk\philemon-typewise-vast\test'.

(SmalltalkMTExporter for: PhilemonTerminalViewTest) exportToFile.
(SmalltalkMTExporter for: PhilemonTerminalClientTest) exportToFile.
