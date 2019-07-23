#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

"EXTENSIONS AND CLASSES******************************************"

"Extension to Filestream for handling reading and writing
 more efficiently as well as to shorten other blocks"
FileStream extend 
[
    |bBuff bNum|

    bFlush 
    [
        |char|
        (bNum = 8) ifFalse: 
        [
            char := Character value: bBuff asInteger.
            self nextPut: char.
            bNum := 8.
            bBuff := 0.
        ]
    ]

    writeBit: bit 
    [
        (bBuff isNil) ifTrue: 
        [
            bNum := 8.
            bBuff := 0.
        ].
        (bNum = 0) ifTrue: [self bFlush].
        bBuff := bBuff bitAt: bNum put: bit.
        bNum := bNum - 1.
    ]

    bitChar: char 
    [
        |bit bitP charV|
        bitP := 8.
        charV := char value.
        [bitP ~= 0] whileTrue: 
        [
            bit := charV bitAt: bitP.
            bitP := bitP - 1.
            self writeBit: bit.
        ].
        ((charV = 0) | (charV = 256)) ifTrue: 
        [
            self writeBit: (charV bitAt: 9).
        ].
    ]

    nextBit 
    [
        |bit|
        (bNum isNil) ifTrue: 
        [
            bNum := 0.
        ].
        (bNum = 0) ifTrue: 
        [
            bNum := 8.
            bBuff := self next.
        ].
        (bBuff isNil) ifTrue: [ ^ nil ].
        bit := bBuff value bitAt: bNum.
        bNum := bNum - 1.
        ^ bit.
    ]

    compress: outfile with: codetable 
    [
        |key code bit|
        [self atEnd not] whileTrue: 
        [
            key := self next value.
            code := codetable at: key.
            code do: 
            [:char |
                bit := char asBit.
                outfile writeBit: bit.
            ].
        ].
        code := codetable at: 256.
        code do: 
        [:char |
            bit := char asBit.
            outfile writeBit: bit.
        ].
        outfile bFlush.
    ]

    decompress: outfile with: tree 
    [
        |activeTree bit|
        activeTree := tree.
        bit := 0.
        [bit ~= nil] whileTrue: 
        [
            bit := self nextBit.
            (bit = 1) ifTrue: 
            [
                activeTree := activeTree right.
            ].
            (bit = 0) ifTrue: 
            [
                activeTree := activeTree left.
            ].
            (activeTree isMemberOf: Leaf) ifTrue: 
            [
                |char|
                (activeTree char = 256) ifTrue: [ ^ char ].
                char := Character value: activeTree char asInteger.
                activeTree := tree.
                outfile nextPut: char.
            ].
        ].
    ]
]

"Stack object."
Object subclass: Stack 
[
    |stackArray top stackItem|

    nl := Character nl.

    Stack class >> new 
    [
        self shouldNotImplement.
    ]

    Stack class >> new: size 
    [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    size 
    [
        ^ top.
    ]

    init: size 
    [
        top := 0.
        stackArray := Array new: size.
    ]

    push: stackItem 
    [
        top := top + 1.
        (top > self size) ifTrue: 
        [
            ^ stderr << 'stack overflow' << nl.
        ].
        stackArray at: top put: stackItem.
    ]

    pop 
    [
        (top = 0) ifTrue: 
        [ 
            stderr << 'stack underflow' << nl.
            ^ nil
        ].
        stackItem := stackArray at: top.
        top := top - 1.
        ^ stackItem
    ]
]

"Frequency table array object."
Object subclass: ZeroArray 
[
    |stackArray|

    ZeroArray class >> new 
    [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size 
    [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size 
    [
        stackArray := Array new: size.
    ]

    size 
    [
        ^ stackArray size.
    ]

    at: index 
    [
        ^ stackArray at: index + 1.
    ]

    at: index put: value 
    [
        ^ stackArray at: index + 1 put: value.
    ]

    incr: index 
    [
        ((self at: index) isNil) ifTrue: 
            [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock 
    [
        (0 to: (self size) - 1) do: 
        [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]

"Print character/equivalent ASCII code."
Character extend 
[
    asBit 
    [
        ^ self value bitAt: 1.
    ]

    visible 
    [
        (($! <= self) & (self <= $~)) ifTrue: [^ self asString]
        ifFalse: [^ self asciiValue printStringRadix: 16]
    ]
]

"Leaf object."
Object subclass: Leaf 
[
    |char count|
    char [ ^ char ]
    count [ ^ count ]

    Leaf class >> new 
    [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount 
    [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar andCount: aCount 
    [
        char := aChar.
        count := aCount.
    ]

    <= other 
    [
        ^ (count < other count)
        | ((count = other count) & (char <= other char))
    ]

    printBase: aStream 
    [
        ^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream 
    [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string 
    [
        visitor value: char value: string.
    ]

    writeIn: outfile 
    [
        outfile writeBit: (0 bitAt: 1).
        outfile bitChar: self char.
    ]
]

"Leaf tree object."
Leaf subclass: Tree 
[
    |left right|
    left [ ^ left ]
    right [ ^ right ]

    Tree class >> new: aChar count: aCount 
    [
        self shouldNotImplement
    ]

    Tree class >> new: aChar count: aCount left: aLeft right: aRight 
    [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]

    setLeft: aLeft andRight: aRight 
    [
        left := aLeft.
        right := aRight.
    ]

    printOn: aStream 
    [
        (self printBase: aStream) << ',' << left << ',' << right 
                                  << ')'.
    ]

    inorder: visitor prefix: string 
    [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]

    writeIn: outfile 
    [
        left writeIn: outfile.
        right writeIn: outfile.
        outfile writeBit: (1 bitAt: 1).
    ]
]

"BEGIN WORKSPACE*************************************************"

"Set program file arguments."
choice := (Smalltalk getArgv: 1).
fileIn := (Smalltalk getArgv: 2).
fileOut := (Smalltalk getArgv: 3).

"Check if proper number of arguments"
(Smalltalk arguments size < 2) ifTrue: 
[ 
    stderr << 'Usage: hzip.st -dtcu inputfile [outputfile]' << nl.
    ObjectMemory quit.
].

((choice includes: $t) | (choice includes: $c)) ifTrue: 
[
    freqtable := ZeroArray new: 257.
    pqueue := SortedCollection new.

    "Load freq table"
    [
        |infile charNum|
        infile := FileStream open: fileIn
                             mode: FileStream read.
        [infile atEnd not] whileTrue: 
        [
            charNum := infile next asInteger.
            freqtable incr: charNum.
        ].
        (charNum = nil) ifFalse: 
        [
            freqtable incr: 256.
        ].
        infile close.
    ] on: SystemExceptions.FileError do: 
    [:signal |
        |errnum|
        errnum := File errno.
        stdout flush.
        stderr << 'hzip.st: ' << fileIn << ': '
               << signal messageText << nl.
        stderr flush.
    ].

    "Load pqueue from freq table"
    freqtable keysAndValuesDo: 
    [:key :value |
        (value notNil and: [value > 0]) ifTrue: 
        [
            newLeaf := Leaf new: (key asCharacter) count: value.
            pqueue add: newLeaf.
        ]
    ].

    "pqueue to tree"
    [pqueue notEmpty] whileTrue: 
    [
        tree := pqueue removeFirst.
        (pqueue size = 0) ifTrue: [^ tree].
        treetwo := pqueue removeFirst.
        newTree := Tree new: tree char 
        count: (tree count + treetwo count)
        left: tree right: treetwo.
        pqueue add: newTree.
    ].
].

"Putting codes into code table from tree"
((choice includes: $t) | (choice includes: $c)) ifTrue: 
[
    codetable := ZeroArray new: 257.
    (tree = nil) ifFalse: 
    [
        tree inorder: 
        [:char :code |
            codetable at: char asInteger put: code.
        ] prefix: ''.
    ].

    "Character, Value, Ct Printing"
    (choice includes: $t) ifTrue: 
    [
        freqtable keysAndValuesDo: 
        [:key :value |
            (value notNil and: [value > 0]) ifTrue: 
            [
                 |character|
                 character := (key asCharacter visible).
                 spacecushion := character asString size.
                 spacecushion := spacecushion + 1.
                 stdout << character. 
                 spacecushion to: 3 do: [:skip| stdout << ' '].
                 spacecushion := value asString size.
                 spacecushion := spacecushion + 1.
                 stdout << value << ' '.
                 spacecushion to: 5 do: [:skip| stdout << ' '].
                 stdout << (codetable at: key) << nl.
            ]
        ]
    ].

    "Compression: in -> out"
    (choice includes: $c) ifTrue: 
    [
        [
            |infile outfile|
            infile := FileStream open: fileIn
                                 mode: FileStream read.
            (fileOut = nil) ifTrue: 
            [
                outfile := stdout.
            ]
            ifFalse: 
            [
                outfile := FileStream open: fileOut
                                      mode: FileStream write.
            ].
            (tree = nil) ifFalse: 
            [
                tree writeIn: outfile.
                outfile writeBit: (1 bitAt: 1).
                outfile bFlush.
                infile compress: outfile with: codetable.
            ].
            outfile close.
            infile close.
        ] on: SystemExceptions.FileError do: [:signal |
            |errno|
            errno := File errno.
            stdout flush.
            stderr << 'hzip.st: ' << fileOut << ': '
                   << signal messageText << nl.
            stderr flush.
        ].
    ].
].

"DecompressionL: in -> out"
(choice includes: $u) ifTrue:
[
    [
        |infile outfile stack preBit controlLoop|
        stack := Stack new: 256.
        controlLoop := 1.
        infile := FileStream open: fileIn
                             mode: FileStream read.
        [(infile atEnd not) & (controlLoop = 1)] whileTrue: 
        [
            preBit := infile nextBit.
            (preBit = 0) ifTrue: 
            [
                |bit bitP bitChar newLeaf controlEnd|
                bitP := 8.
                bitChar := 0.
                controlEnd := 0.
                [bitP ~= 0] whileTrue: 
                [
                    bit := infile nextBit.
                    bitChar := bitChar bitAt: bitP put: bit.
                    bitP := bitP - 1.
                ].
                (bitChar = 0) ifTrue: 
                [
                    bit := infile nextBit.
                    (bit = 1) ifTrue: 
                    [
                        controlEnd := 1.
                    ].
                ].
                (controlEnd = 0) ifTrue:
                [
                    newLeaf := Leaf new: (bitChar asCharacter) count: 0.
                ].
                (controlEnd = 0) ifFalse:
                [
                    newLeaf := Leaf new: 256 count: 0.
                ].
                stack push: newLeaf.
            ].
            (preBit = 1) ifTrue: 
            [
                |newTree treeone treetwo|
                treetwo := stack pop.
                (stack size = 0) ifTrue: 
                [
                    bNum := 0.
                    controlLoop := 0.
                    tree := treetwo.
                ].
                (controlLoop = 1) ifTrue:
                [
                    treeone := stack pop.
                    newTree := Tree new: treeone char 
                    count: 0 left: treeone right: treetwo.
                    stack push: newTree.
                ].
            ].
        ].
        (tree = nil) ifFalse: 
        [
            (fileOut = nil) ifTrue: 
            [
                infile decompress: stdout with: tree.
            ]
            ifFalse: 
            [
                outfile := FileStream open: fileOut
                                      mode: FileStream write.
                infile decompress: outfile with: tree.
                outfile close.
            ].
        ].
        infile close.
    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << 'hzip.st: ' << fileIn << ': '
               << signal messageText << nl.
        stderr flush.
    ].
].

