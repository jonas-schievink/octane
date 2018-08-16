//! Stores static information about kernel exports.

use kernel::types::*;

/// A symbol exported by the kernel (either a shared variable or a kernel API
/// function).
#[derive(Debug)]
pub struct KernelExport {
    /// Symbol name.
    pub name: &'static str,
    /// ID used to import the symbol.
    pub ordinal: u16,
    pub kind: KernelExportKind,
}

#[derive(Debug)]
pub enum KernelExportKind {
    /// Callable API function.
    Function {
        /// The ABI used by the function.
        abi: KernelAbi,
        /// Number of arguments the function takes.
        arity: u32,
    },
    /// Shared variable stored in the kernel memory of the Xbox address space
    /// (>2G area).
    Variable {
        /// Number of bytes to allocate.
        size: u32,
        /// Alignment needed by the host system.
        align: u32,
    },
    /// Unused entry.
    ///
    /// Attempts to use are game bugs or CPU emulation bugs.
    Unused,
    /// Not yet implemented, but known to exist.
    ///
    /// Attempts to use are emulator bugs.
    Unimplemented,
}

/// Kernel function ABI / calling convention.
#[derive(Debug)]
pub enum KernelAbi {
    /// Parameters are passed using the stack and pushed in right-to-left order.
    ///
    /// Callee cleans up the stack, usually via a `ret N` instruction.
    ///
    /// Returns value is passed in `eax`.
    Stdcall,
    /// First 2 arguments (left to right) are passed via `ecx` and `edx`. Other
    /// arguments are passed like in `stdcall` (pushed to the stack, right to
    /// left).
    #[allow(unused)]    // TODO remove
    Fastcall,
}

/// Defines a `KernelExport` denoting a kernel function.
macro_rules! func {
    ($ord:tt: $name:ident abi=$abi:ident arity=$arity:tt) => {
        KernelExport {
            name: stringify!($name),
            ordinal: $ord,
            kind: KernelExportKind::Function {
                abi: KernelAbi::$abi,
                arity: $arity,
            },
        }
    };
}

macro_rules! var {
    ($ord:tt: $name:ident: $t:ty) => {
        KernelExport {
            name: stringify!($name),
            ordinal: $ord,
            kind: KernelExportKind::Variable {
                size: <$t as KernelVariable>::SIZE,
                align: <$t as KernelVariable>::ALIGN,
            },
        }
    };
}

/// Defines a `KernelExport` denoting an unused export ID.
macro_rules! unused {
    ($ord:tt) => {
        KernelExport {
            name: concat!("<unused #", $ord, ">"),
            ordinal: $ord,
            kind: KernelExportKind::Unused,
        }
    };
}

/// Defines a `KernelExport` for a currently unimplemented function or variable.
macro_rules! unimp {
    ($ord:tt) => {
        KernelExport {
            name: concat!("<unimplemented #", $ord, ">"),
            ordinal: $ord,
            kind: KernelExportKind::Unimplemented,
        }
    };
    ($ord:tt: $name:tt) => {
        KernelExport {
            name: stringify!($name),
            ordinal: $ord,
            kind: KernelExportKind::Unimplemented,
        }
    }
}

static EXPORT_TABLE: &'static [KernelExport] = &[
    unused! (0),
    func!   (1: AvGetSavedDataAddress            abi=Stdcall arity=0),
    func!   (2: AvSendTVEncoderOption            abi=Stdcall arity=4),
    func!   (3: AvSetDisplayMode                 abi=Stdcall arity=6),
    func!   (4: AvSetSavedDataAddress            abi=Stdcall arity=1),
    unimp!  (5: DbgBreakPoint),
    unimp!  (6: DbgBreakPointWithStatus),
    unimp!  (7: DbgLoadImageSymbols),
    unimp!  (8: DbgPrint),
    func!   (9: HalReadSMCTrayState              abi=Stdcall arity=2),
    unimp! (10: DbgPrompt),
    unimp! (11: DbgUnLoadImageSymbols),
    unimp! (12: ExAcquireReadWriteLockExclusive),
    unimp! (13: ExAcquireReadWriteLockShared),
    func!  (14: ExAllocatePool                   abi=Stdcall arity=1),
    unimp! (15: ExAllocatePoolWithTag),
    unimp! (16: ExEventObjectType),
    func!  (17: ExFreePool                       abi=Stdcall arity=1),
    unimp! (18: ExInitializeReadWriteLock),
    unimp! (19: ExInterlockedAddLargeInteger),
    unimp! (20: ExInterlockedAddLargeStatistic),
    unimp! (21: ExInterlockedCompareExchange64),
    unimp! (22: ExMutantObjectType),
    unimp! (23: ExQueryPoolBlockSize),
    unimp! (24: ExQueryNonVolatileSetting),
    unimp! (25: ExReadWriteRefurbInfo),
    unimp! (26: ExRaiseException),
    unimp! (27: ExRaiseStatus),
    unimp! (28: ExReleaseReadWriteLock),
    unimp! (29: ExSaveNonVolatileSetting),
    unimp! (30: ExSemaphoreObjectType),
    unimp! (31: ExTimerObjectType),
    unimp! (32: ExfInterlockedInsertHeadList),
    unimp! (33: ExfInterlockedInsertTailList),
    unimp! (34: ExfInterlockedRemoveHeadList),
    unimp! (35: FscGetCacheSize),
    unimp! (36: FscInvalidateIdleBlocks),
    unimp! (37: FscSetCacheSize),
    unimp! (38: HalClearSoftwareInterrupt),
    unimp! (39: HalDisableSystemInterrupt),
    var!   (40: HalDiskCachePartitionCount: Ulong),
    unimp! (41: HalDiskModelNumber),
    unimp! (42: HalDiskSerialNumber),
    unimp! (43: HalEnableSystemInterrupt),
    unimp! (44: HalGetInterruptVector),
    unimp! (45: HalReadSMBusValue),
    unimp! (46: HalReadWritePCISpace),
    unimp! (47: HalRegisterShutdownNotification),
    unimp! (48: HalRequestSoftwareInterrupt),
    unimp! (49: HalReturnToFirmware),
    unimp! (50: HalWriteSMBusValue),
    unimp! (51: InterlockedCompareExchange),
    unimp! (52: InterlockedDecrement),
    unimp! (53: InterlockedIncrement),
    unimp! (54: InterlockedExchange),
    unimp! (55: InterlockedExchangeAdd),
    unimp! (56: InterlockedFlushSList),
    unimp! (57: InterlockedPopEntrySList),
    unimp! (58: InterlockedPushEntrySList),
    unimp! (59: IoAllocateIrp),
    unimp! (60: IoBuildAsynchronousFsdRequest),
    unimp! (61: IoBuildDeviceIoControlRequest),
    unimp! (62: IoBuildSynchronousFsdRequest),
    unimp! (63: IoCheckShareAccess),
    unimp! (64: IoCompletionObjectType),
    unimp! (65: IoCreateDevice),
    unimp! (66: IoCreateFile),
    unimp! (67: IoCreateSymbolicLink),
    unimp! (68: IoDeleteDevice),
    unimp! (69: IoDeleteSymbolicLink),
    unimp! (70: IoDeviceObjectType),
    unimp! (71: IoFileObjectType),
    unimp! (72: IoFreeIrp),
    unimp! (73: IoInitializeIrp),
    unimp! (74: IoInvalidDeviceRequest),
    unimp! (75: IoQueryFileInformation),
    unimp! (76: IoQueryVolumeInformation),
    unimp! (77: IoQueueThreadIrp),
    unimp! (78: IoRemoveShareAccess),
    unimp! (79: IoSetIoCompletion),
    unimp! (80: IoSetShareAccess),
    unimp! (81: IoStartNextPacket),
    unimp! (82: IoStartNextPacketByKey),
    unimp! (83: IoStartPacket),
    unimp! (84: IoSynchronousDeviceIoControlRequest),
    unimp! (85: IoSynchronousFsdRequest),
    unimp! (86: IofCallDriver),
    unimp! (87: IofCompleteRequest),
    unimp! (88: KdDebuggerEnabled),
    unimp! (89: KdDebuggerNotPresent),
    unimp! (90: IoDismountVolume),
    unimp! (91: IoDismountVolumeByName),
    unimp! (92: KeAlertResumeThread),
    unimp! (93: KeAlertThread),
    unimp! (94: KeBoostPriorityThread),
    unimp! (95: KeBugCheck),
    unimp! (96: KeBugCheckEx),
    unimp! (97: KeCancelTimer),
    unimp! (98: KeConnectInterrupt),
    unimp! (99: KeDelayExecutionThread),
    unimp!(100: KeDisconnectInterrupt),
    unimp!(101: KeEnterCriticalRegion),
    unimp!(102: MmGlobalData),
    unimp!(103: KeGetCurrentIrql),
    unimp!(104: KeGetCurrentThread),
    unimp!(105: KeInitializeApc),
    unimp!(106: KeInitializeDeviceQueue),
    unimp!(107: KeInitializeDpc),
    unimp!(108: KeInitializeEvent),
    unimp!(109: KeInitializeInterrupt),
    unimp!(110: KeInitializeMutant),
    unimp!(111: KeInitializeQueue),
    unimp!(112: KeInitializeSemaphore),
    unimp!(113: KeInitializeTimerEx),
    unimp!(114: KeInsertByKeyDeviceQueue),
    unimp!(115: KeInsertDeviceQueue),
    unimp!(116: KeInsertHeadQueue),
    unimp!(117: KeInsertQueue),
    unimp!(118: KeInsertQueueApc),
    unimp!(119: KeInsertQueueDpc),
    unimp!(120: KeInterruptTime),
    unimp!(121: KeIsExecutingDpc),
    unimp!(122: KeLeaveCriticalRegion),
    unimp!(123: KePulseEvent),
    unimp!(124: KeQueryBasePriorityThread),
    unimp!(125: KeQueryInterruptTime),
    unimp!(126: KeQueryPerformanceCounter),
    unimp!(127: KeQueryPerformanceFrequency),
    unimp!(128: KeQuerySystemTime),
    unimp!(129: KeRaiseIrqlToDpcLevel),
    unimp!(130: KeRaiseIrqlToSynchLevel),
    unimp!(131: KeReleaseMutant),
    unimp!(132: KeReleaseSemaphore),
    unimp!(133: KeRemoveByKeyDeviceQueue),
    unimp!(134: KeRemoveDeviceQueue),
    unimp!(135: KeRemoveEntryDeviceQueue),
    unimp!(136: KeRemoveQueue),
    unimp!(137: KeRemoveQueueDpc),
    unimp!(138: KeResetEvent),
    unimp!(139: KeRestoreFloatingPointState),
    unimp!(140: KeResumeThread),
    unimp!(141: KeRundownQueue),
    unimp!(142: KeSaveFloatingPointState),
    unimp!(143: KeSetBasePriorityThread),
    unimp!(144: KeSetDisableBoostThread),
    unimp!(145: KeSetEvent),
    unimp!(146: KeSetEventBoostPriority),
    unimp!(147: KeSetPriorityProcess),
    unimp!(148: KeSetPriorityThread),
    unimp!(149: KeSetTimer),
    unimp!(150: KeSetTimerEx),
    unimp!(151: KeStallExecutionProcessor),
    unimp!(152: KeSuspendThread),
    unimp!(153: KeSynchronizeExecution),
    unimp!(154: KeSystemTime),
    unimp!(155: KeTestAlertThread),
    var!  (156: KeTickCount: Ulong),
    var!  (157: KeTimeIncrement: Ulong),
    unimp!(158: KeWaitForMultipleObjects),
    unimp!(159: KeWaitForSingleObject),
    unimp!(160: KfRaiseIrql),
    unimp!(161: KfLowerIrql),
    unimp!(162: KiBugCheckData),
    unimp!(163: KiUnlockDispatcherDatabase),
    unimp!(164: LaunchDataPage),
    unimp!(165: MmAllocateContiguousMemory),
    unimp!(166: MmAllocateContiguousMemoryEx),
    unimp!(167: MmAllocateSystemMemory),
    unimp!(168: MmClaimGpuInstanceMemory),
    unimp!(169: MmCreateKernelStack),
    unimp!(170: MmDeleteKernelStack),
    unimp!(171: MmFreeContiguousMemory),
    unimp!(172: MmFreeSystemMemory),
    unimp!(173: MmGetPhysicalAddress),
    unimp!(174: MmIsAddressValid),
    unimp!(175: MmLockUnlockBufferPages),
    unimp!(176: MmLockUnlockPhysicalPage),
    unimp!(177: MmMapIoSpace),
    unimp!(178: MmPersistContiguousMemory),
    unimp!(179: MmQueryAddressProtect),
    unimp!(180: MmQueryAllocationSize),
    unimp!(181: MmQueryStatistics),
    unimp!(182: MmSetAddressProtect),
    unimp!(183: MmUnmapIoSpace),
    unimp!(184: NtAllocateVirtualMemory),
    unimp!(185: NtCancelTimer),
    unimp!(186: NtClearEvent),
    unimp!(187: NtClose),
    unimp!(188: NtCreateDirectoryObject),
    unimp!(189: NtCreateEvent),
    unimp!(190: NtCreateFile),
    unimp!(191: NtCreateIoCompletion),
    unimp!(192: NtCreateMutant),
    unimp!(193: NtCreateSemaphore),
    unimp!(194: NtCreateTimer),
    unimp!(195: NtDeleteFile),
    unimp!(196: NtDeviceIoControlFile),
    unimp!(197: NtDuplicateObject),
    unimp!(198: NtFlushBuffersFile),
    unimp!(199: NtFreeVirtualMemory),
    unimp!(200: NtFsControlFile),
    unimp!(201: NtOpenDirectoryObject),
    unimp!(202: NtOpenFile),
    unimp!(203: NtOpenSymbolicLinkObject),
    unimp!(204: NtProtectVirtualMemory),
    unimp!(205: NtPulseEvent),
    unimp!(206: NtQueueApcThread),
    unimp!(207: NtQueryDirectoryFile),
    unimp!(208: NtQueryDirectoryObject),
    unimp!(209: NtQueryEvent),
    unimp!(210: NtQueryFullAttributesFile),
    unimp!(211: NtQueryInformationFile),
    unimp!(212: NtQueryIoCompletion),
    unimp!(213: NtQueryMutant),
    unimp!(214: NtQuerySemaphore),
    unimp!(215: NtQuerySymbolicLinkObject),
    unimp!(216: NtQueryTimer),
    unimp!(217: NtQueryVirtualMemory),
    unimp!(218: NtQueryVolumeInformationFile),
    unimp!(219: NtReadFile),
    unimp!(220: NtReadFileScatter),
    unimp!(221: NtReleaseMutant),
    unimp!(222: NtReleaseSemaphore),
    unimp!(223: NtRemoveIoCompletion),
    unimp!(224: NtResumeThread),
    unimp!(225: NtSetEvent),
    unimp!(226: NtSetInformationFile),
    unimp!(227: NtSetIoCompletion),
    unimp!(228: NtSetSystemTime),
    unimp!(229: NtSetTimerEx),
    unimp!(230: NtSignalAndWaitForSingleObjectEx),
    unimp!(231: NtSuspendThread),
    unimp!(232: NtUserIoApcDispatcher),
    unimp!(233: NtWaitForSingleObject),
    unimp!(234: NtWaitForSingleObjectEx),
    unimp!(235: NtWaitForMultipleObjectsEx),
    unimp!(236: NtWriteFile),
    unimp!(237: NtWriteFileGather),
    unimp!(238: NtYieldExecution),
    unimp!(239: ObCreateObject),
    unimp!(240: ObDirectoryObjectType),
    unimp!(241: ObInsertObject),
    unimp!(242: ObMakeTemporaryObject),
    unimp!(243: ObOpenObjectByName),
    unimp!(244: ObOpenObjectByPointer),
    unimp!(245: ObpObjectHandleTable),
    unimp!(246: ObReferenceObjectByHandle),
    unimp!(247: ObReferenceObjectByName),
    unimp!(248: ObReferenceObjectByPointer),
    unimp!(249: ObSymbolicLinkObjectType),
    unimp!(250: ObfDereferenceObject),
    unimp!(251: ObfReferenceObject),
    unimp!(252: PhyGetLinkState),
    unimp!(253: PhyInitialize),
    unimp!(254: PsCreateSystemThread),
    func! (255: PsCreateSystemThreadEx           abi=Stdcall arity=10),
    unimp!(256: PsQueryStatistics),
    unimp!(257: PsSetCreateThreadNotifyRoutine),
    unimp!(258: PsTerminateSystemThread),
    unimp!(259: PsThreadObjectType),
    unimp!(260: RtlAnsiStringToUnicodeString),
    unimp!(261: RtlAppendStringToString),
    unimp!(262: RtlAppendUnicodeStringToString),
    unimp!(263: RtlAppendUnicodeToString),
    unimp!(264: RtlAssert),
    unimp!(265: RtlCaptureContext),
    unimp!(266: RtlCaptureStackBackTrace),
    unimp!(267: RtlCharToInteger),
    unimp!(268: RtlCompareMemory),
    unimp!(269: RtlCompareMemoryUlong),
    unimp!(270: RtlCompareString),
    unimp!(271: RtlCompareUnicodeString),
    unimp!(272: RtlCopyString),
    unimp!(273: RtlCopyUnicodeString),
    unimp!(274: RtlCreateUnicodeString),
    unimp!(275: RtlDowncaseUnicodeChar),
    unimp!(276: RtlDowncaseUnicodeString),
    unimp!(277: RtlEnterCriticalSection),
    unimp!(278: RtlEnterCriticalSectionAndRegion),
    unimp!(279: RtlEqualString),
    unimp!(280: RtlEqualUnicodeString),
    unimp!(281: RtlExtendedIntegerMultiply),
    unimp!(282: RtlExtendedLargeIntegerDivide),
    unimp!(283: RtlExtendedMagicDivide),
    unimp!(284: RtlFillMemory),
    unimp!(285: RtlFillMemoryUlong),
    unimp!(286: RtlFreeAnsiString),
    unimp!(287: RtlFreeUnicodeString),
    unimp!(288: RtlGetCallersAddress),
    unimp!(289: RtlInitAnsiString),
    unimp!(290: RtlInitUnicodeString),
    unimp!(291: RtlInitializeCriticalSection),
    unimp!(292: RtlIntegerToChar),
    unimp!(293: RtlIntegerToUnicodeString),
    unimp!(294: RtlLeaveCriticalSection),
    unimp!(295: RtlLeaveCriticalSectionAndRegion),
    unimp!(296: RtlLowerChar),
    unimp!(297: RtlMapGenericMask),
    unimp!(298: RtlMoveMemory),
    unimp!(299: RtlMultiByteToUnicodeN),
    unimp!(300: RtlMultiByteToUnicodeSize),
    unimp!(301: RtlNtStatusToDosError),
    unimp!(302: RtlRaiseException),
    unimp!(303: RtlRaiseStatus),
    unimp!(304: RtlTimeFieldsToTime),
    unimp!(305: RtlTimeToTimeFields),
    unimp!(306: RtlTryEnterCriticalSection),
    unimp!(307: RtlUlongByteSwap),
    unimp!(308: RtlUnicodeStringToAnsiString),
    unimp!(309: RtlUnicodeStringToInteger),
    unimp!(310: RtlUnicodeToMultiByteN),
    unimp!(311: RtlUnicodeToMultiByteSize),
    unimp!(312: RtlUnwind),
    unimp!(313: RtlUpcaseUnicodeChar),
    unimp!(314: RtlUpcaseUnicodeString),
    unimp!(315: RtlUpcaseUnicodeToMultiByteN),
    unimp!(316: RtlUpperChar),
    unimp!(317: RtlUpperString),
    unimp!(318: RtlUshortByteSwap),
    unimp!(319: RtlWalkFrameChain),
    unimp!(320: RtlZeroMemory),
    unimp!(321: XboxEEPROMKey),
    unimp!(322: XboxHardwareInfo),
    unimp!(323: XboxHDKey),
    unimp!(324: XboxKrnlVersion),
    unimp!(325: XboxSignatureKey),
    unimp!(326: XeImageFileName),
    unimp!(327: XeLoadSection),
    unimp!(328: XeUnloadSection),
    unimp!(329: READ_PORT_BUFFER_UCHAR),
    unimp!(330: READ_PORT_BUFFER_USHORT),
    unimp!(331: READ_PORT_BUFFER_ULONG),
    unimp!(332: WRITE_PORT_BUFFER_UCHAR),
    unimp!(333: WRITE_PORT_BUFFER_USHORT),
    unimp!(334: WRITE_PORT_BUFFER_ULONG),
    unimp!(335: XcSHAInit),
    unimp!(336: XcSHAUpdate),
    unimp!(337: XcSHAFinal),
    unimp!(338: XcRC4Key),
    unimp!(339: XcRC4Crypt),
    unimp!(340: XcHMAC),
    unimp!(341: XcPKEncPublic),
    unimp!(342: XcPKDecPrivate),
    unimp!(343: XcPKGetKeyLen),
    unimp!(344: XcVerifyPKCS1Signature),
    unimp!(345: XcModExp),
    unimp!(346: XcDESKeyParity),
    unimp!(347: XcKeyTable),
    unimp!(348: XcBlockCrypt),
    unimp!(349: XcBlockCryptCBC),
    unimp!(350: XcCryptService),
    unimp!(351: XcUpdateCrypto),
    unimp!(352: RtlRip),
    unimp!(353: XboxLANKey),
    unimp!(354: XboxAlternateSignatureKeys),
    unimp!(355: XePublicKeyData),
    unimp!(356: HalBootSMCVideoMode),
    unimp!(357: IdexChannelObject),
    unimp!(358: HalIsResetOrShutdownPending),
    unimp!(359: IoMarkIrpMustComplete),
    unimp!(360: HalInitiateShutdown),
    unimp!(361: RtlSnprintf),
    unimp!(362: RtlSprintf),
    unimp!(363: RtlVsnprintf),
    unimp!(364: RtlVsprintf),
    unimp!(365: HalEnableSecureTrayEject),
    unimp!(366: HalWriteSMCScratchRegister),
    unused!(367),
    unused!(368),
    unused!(369),
    unimp!(370: XProfpControl),
    unimp!(371: XProfpGetData),
    unimp!(372: IrtClientInitFast),
    unimp!(373: IrtSweep),
    unimp!(374: MmDbgAllocateMemory),
    unimp!(375: MmDbgFreeMemory),
    unimp!(376: MmDbgQueryAvailablePages),
    unimp!(377: MmDbgReleaseAddress),
    unimp!(378: MmDbgWriteCheck),
];

const MAX_USED_ORDINAL: u16 = 378;

/// Retrieve kernel export information given the export ID / ordinal.
///
/// The same ordinal is also stored in the XBE file's thunk table.
pub fn get_export_info(mut ordinal: u16) -> &'static KernelExport {
    static UNIMP: KernelExport = unimp!(0);

    if ordinal > MAX_USED_ORDINAL {
        ordinal = 0;    // unused
    }
    EXPORT_TABLE.get(ordinal as usize).unwrap_or(&UNIMP)
}

/// Checks that ordinals are allocated in sequence.
#[test]
fn ordinals() {
    for (ord, export) in EXPORT_TABLE.iter().enumerate() {
        assert_eq!(ord, export.ordinal as usize,
                   "{} has an incorrect ordinal: should be {}, is {}",
                   export.name, ord, export.ordinal);
    }
}
