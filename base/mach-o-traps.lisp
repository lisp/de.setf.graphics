
;;; a first pass at a #_trap interface to os-x framework entries.
;;; james.nderson@setf.de
;;; incorporating bundles.lisp/gb@clozure.com, and suggestions from john@louch.com
;;; derived from the mcl-interface/{wiseman@neodesic.com,alexander@agentsheets.com} opengl interface
;;; and the mcl new-traps.lisp ppc trap implementation.
;;;
;;; 20030910.jaa ...
;;; 20030911.jaa grouped entry resolution, CFBundleGetBundleWithIdentifier in addition to CFBundleLoadExecutable
;;; 20030917.jaa added spreading record argument values and interposing a return value pointer
;;; 20030917     included changes to translation to handle struct specifications
;;; 20030917     see http://developer.apple.com/documentation/DeveloperTools/Conceptual/MachORuntime/2rt_powerpc_abi/index.html
;;; 20030924.jaa added if-exists/if-does-not-exist precedding to mach-o-entry in order to allow deletion
;;; 20030926.jaa added %setr macro
;;; 20040111.jaa added update of entry point name to mach-o-entry
;;; 20040201.jaa added distinct mach-o-bundle operator with distinct internal and external name and a searchpath
;;;              which is cached in the bundle description and subsequently used to locate the library.
;;; 20060330.jaa don't release results from CFURLGetString. the follow the "get rule".

(in-package :ccl)

(export '(mach-o-function
          undefined-bundle-entry
          undefined-bundle)
        :ccl)
(import '(mach-o-function)
        :traps)


(define-condition undefined-bundle-entry (control-error undefined-function)
  ((entry-name :initarg :entry-name :reader undefined-function-entry-name)
   (library-name :initarg :library-name :reader undefined-function-library-name))
  (:report (lambda (c s) (format s "Undefined bundle entry ~a.~a called."
                                 (undefined-function-entry-name c)
                                 (undefined-function-library-name c)))))

(define-condition undefined-bundle (control-error)
  ((library-name :initarg :library-name :reader undefined-bundle-library-name)
   (cause :initarg :cause :reader undefined-bundle-cause))
  (:report (lambda (c s) (format s "Undefined bundle ~s.~%~a"
                                 (undefined-bundle-library-name c)
                                 (undefined-bundle-cause c)))))

;;;
;;; mach-o bundle and entry modeling
;;; each entry is allocated a named cache for the entry id value which is identified by trap name.
;;; the first time it is referenced, as in the ppc-ff-call form, the id is resolved.
;;; when an image is saved, the id values are cleared.

(defvar *mach-o-bundles* (make-hash-table :test 'equal)
  "binds a hashtable of bundle descriptions.")

(unless (ignore-errors (logical-pathname-translations "SYS"))
  (setf (logical-pathname-translations "SYS") nil))

(defvar *default-framework-location*
  (logical-pathname "SYS:FRAMEWORKS;ANY;FRAMEWORKS;"))

(defvar *initial-framework-search-path*
  (list (logical-pathname "SYS:FRAMEWORKS;USER;FRAMEWORKS;")
        (logical-pathname "SYS:FRAMEWORKS;APPLICATION;FRAMEWORKS;")
        *default-framework-location*)
  "binds the initial framework search path.
   the logical pathname translations for these are managed automatically.")

(defvar *framework-search-path* *initial-framework-search-path*
  "binds the search path used by a mach-o-bundle-description to locate a library.
   the initial value is *initial-framework-search-path*.")

(defstruct (mach-o-entry-description
            (:conc-name "%MOE-")
            (:print-function (lambda (moe stream level)
                               (cond ((and *print-level* level (>= level *print-level*))
                                      (write-char #\# stream))
                                     (t (print-unreadable-object (moe stream)
                                          (format stream "MOE ~a~@[(@~8,'0x)~] <~a#~a>"
                                                  (%moe-trap-name moe) (%moe-id moe)
                                                  (%moe-library-name moe) (%moe-name moe))))))))
  "describes a mach-o entry point. the entry and library name strings and the trap name symbol are supplied when constructed, from the values specified in the respective mach-o-function definition. the id is retrieved from the loaded bundle when the function is first called. all entries for a given library are reachable from the bundle and are resolved together."
  (id nil)
  (name "")
  (trap-name nil)
  (bundle nil))



(defStruct (mach-o-bundle-description
            (:conc-name "%MOB-")
            (:print-function (lambda (mob stream level)
                               (cond ((and *print-level* level (>= level *print-level*))
                                      (write-char #\# stream))
                                     (t (print-unreadable-object (mob stream)
                                          (format stream "MOB ~a with ~s entries~@[, locked: ~s~]"
                                                  (%mob-name mob) (hash-table-count (%mob-entries mob))
                                                  (lock-owner (%mob-lock mob))))))))
            (:constructor make-mach-o-bundle-description (&key name (library-name name) entries lock search-path)))
  "collects the mach-o entry descriptions for the entry points in a given bundle. the enries are stored in a hash-table keyed by trap name - which means a given entry point may appear more than once. each bundle description alsp includes a lock which is used to serialize id resolution. it is not used for definitions."
  (name "")
  (entries (make-hash-table))
  (lock (make-lock))
  (library-name "")
  (search-path *framework-search-path*))


(defun %moe-library-name (moe)
  (let ((mob (%moe-bundle moe)))
    (typecase mob
      (mach-o-bundle-description (%mob-name mob))
      (t ""))))

(defun mach-o-entry-id (moe)
  "return the known entry point id or resolve it on demand."
  (or (%moe-id moe)
      (progn (resolve-bundle-entries (%moe-bundle moe))
             (or (%moe-id moe)
                 (error 'undefined-bundle-entry
                        :entry-name (%moe-name moe) :library-name (%moe-library-name moe))))))



;;; See the sample code at:
;;; http://developer.apple.com/samplecode/Sample_Code/Runtime_Architecture/CallMachOFramework.htm
;;; This is The Official Way to open and use Mach-O libraries from CFM
;;; Carbon applications.  -gb 2/21/02
;;; (Revised slightly, since MCL's interface files now define
;;; more constants and entrypoints than they once did) -gb 11/12/02

(ccl::eval-when (:compile-toplevel :execute)
  (ccl::require-interface :folders)) ; need this to get at following constants


(defparameter *supported-framework-locations*
  ; from Folders.lisp - value is a vRefNum (a signed-integer)
  ; this is an exhaustive list as of MCL 5.0b5
  `((:system-disk . ,traps::$kOnSystemDisk)
    (:appropriate-disk . ,traps::$kOnAppropriateDisk) ;(system disk, not necessarily boot disk)
    (:applications . ,traps::$kApplicationsFolderType)
    ;
    (:system . ,traps::$kSystemDomain) ; typically used when framework is located on system disk (same as :appropriate-disk ?)
    (:local . ,traps::$kLocalDomain)
    (:network . ,traps::$kNetworkDomain)
    (:user . ,traps::$kUserDomain) ; typically used when framework is located in home directory of user
    (:classic . ,traps::$kClassicDomain)))

(defparameter *supported-framework-folder-types*
  ; from Folders.lisp - value is a foldertype (an ostype)
  ; the following is an auto generated exhaustive list of all know folder-types
  `((:DOMAIN-TOP-LEVEL . ,traps::$kDomainTopLevelFolderType)
    (:DOMAIN-LIBRARY . ,traps::$kDomainLibraryFolderType)
    (:COLOR-SYNC . ,traps::$kColorSyncFolderType)
    (:COLOR-SYNC-CMM . ,traps::$kColorSyncCMMFolderType)
    (:COLOR-SYNC-SCRIPTING . ,traps::$kColorSyncScriptingFolderType)
    (:PRINTERS . ,traps::$kPrintersFolderType)
    (:SPEECH . ,traps::$kSpeechFolderType)
    (:CARBON-LIBRARY . ,traps::$kCarbonLibraryFolderType)
    (:DOCUMENTATION . ,traps::$kDocumentationFolderType)
    (:DEVELOPER-DOCS . ,traps::$kDeveloperDocsFolderType)
    (:DEVELOPER-HELP . ,traps::$kDeveloperHelpFolderType)
    (:ISS-DOWNLOADS . ,traps::$kISSDownloadsFolderType)
    (:USER-SPECIFIC-TMP . ,traps::$kUserSpecificTmpFolderType)
    (:CACHED-DATA . ,traps::$kCachedDataFolderType)
    (:FRAMEWORKS . ,traps::$kFrameworksFolderType) ; typically used when framework in folder /system/library/frameworks
    (:PRIVATE-FRAMEWORKS . ,traps::$kPrivateFrameworksFolderType)
    (:CLASSIC-DESKTOP . ,traps::$kClassicDesktopFolderType)
    (:DEVELOPER . ,traps::$kDeveloperFolderType)
    (:SYSTEM-SOUNDS . ,traps::$kSystemSoundsFolderType)
    (:COMPONENTS . ,traps::$kComponentsFolderType)
    (:QUICK-TIME-COMPONENTS . ,traps::$kQuickTimeComponentsFolderType)
    (:CORE-SERVICES . ,traps::$kCoreServicesFolderType)
    (:PICTURE-DOCUMENTS . ,traps::$kPictureDocumentsFolderType)
    (:MOVIE-DOCUMENTS . ,traps::$kMovieDocumentsFolderType)
    (:MUSIC-DOCUMENTS . ,traps::$kMusicDocumentsFolderType)
    (:INTERNET-SITES . ,traps::$kInternetSitesFolderType)
    (:PUBLIC . ,traps::$kPublicFolderType)
    (:AUDIO-SUPPORT . ,traps::$kAudioSupportFolderType)
    (:AUDIO-SOUNDS . ,traps::$kAudioSoundsFolderType)
    (:AUDIO-SOUND-BANKS . ,traps::$kAudioSoundBanksFolderType)
    (:AUDIO-ALERT-SOUNDS . ,traps::$kAudioAlertSoundsFolderType)
    (:AUDIO-PLUG-INS . ,traps::$kAudioPlugInsFolderType)
    (:AUDIO-COMPONENTS . ,traps::$kAudioComponentsFolderType)
    (:KERNEL-EXTENSIONS . ,traps::$kKernelExtensionsFolderType)
    (:DIRECTORY-SERVICES . ,traps::$kDirectoryServicesFolderType)
    (:DIRECTORY-SERVICES-PLUG-INS . ,traps::$kDirectoryServicesPlugInsFolderType)
    (:INSTALLER-RECEIPTS . ,traps::$kInstallerReceiptsFolderType)
    (:FILE-SYSTEM-SUPPORT . ,traps::$kFileSystemSupportFolderType)
    (:APPLE-SHARE-SUPPORT . ,traps::$kAppleShareSupportFolderType)
    (:APPLE-SHARE-AUTHENTICATION . ,traps::$kAppleShareAuthenticationFolderType)
    (:MIDI-DRIVERS . ,traps::$kMIDIDriversFolderType)
    (:LOCALES . ,traps::$kLocalesFolderType)
    (:FIND-BY-CONTENT-PLUGINS . ,traps::$kFindByContentPluginsFolderType)
    (:USERS . ,traps::$kUsersFolderType)
    (:CURRENT-USER . ,traps::$kCurrentUserFolderType) ; typically used when framework in user's home directory
    (:CURRENT-USER-REMOTE . ,traps::$kCurrentUserRemoteFolderType)
    (:SHARED-USER-DATA . ,traps::$kSharedUserDataFolderType)
    (:VOLUME-SETTINGS . ,traps::$kVolumeSettingsFolderType)
    (:APPLESHARE-AUTOMOUNT-SERVER-ALIASES . ,traps::$kAppleshareAutomountServerAliasesFolderType)
    (:PRE-MAC-OS91-APPLICATIONS . ,traps::$kPreMacOS91ApplicationsFolderType)
    (:PRE-MAC-OS91-INSTALLER-LOGS . ,traps::$kPreMacOS91InstallerLogsFolderType)
    (:PRE-MAC-OS91-ASSISTANTS . ,traps::$kPreMacOS91AssistantsFolderType)
    (:PRE-MAC-OS91-UTILITIES . ,traps::$kPreMacOS91UtilitiesFolderType)
    (:PRE-MAC-OS91-APPLE-EXTRAS . ,traps::$kPreMacOS91AppleExtrasFolderType)
    (:PRE-MAC-OS91-MAC-OS-READ-MES . ,traps::$kPreMacOS91MacOSReadMesFolderType)
    (:PRE-MAC-OS91-INTERNET . ,traps::$kPreMacOS91InternetFolderType)
    (:PRE-MAC-OS91-AUTOMOUNTED-SERVERS . ,traps::$kPreMacOS91AutomountedServersFolderType)))

(defparameter *supported-framework-location-logical-paths*
  ; from Folders.lisp - value is a vRefNum (a signed-integer)
  ; this is an exhaustive list as of MCL 5.0b5
  `((:system-disk . ,(make-pathname :host nil :directory '(:relative "SYSTEM-VOLUME")))
    (:appropriate-disk . ,(make-pathname :host nil :directory '(:relative "ANY")))
    (:applications . ,(make-pathname :host nil :directory '(:relative "APPLICATIONS")))
    ;
    (:system . ,(make-pathname :host nil :directory '(:relative "SYSTEM")))
    (:local . ,(make-pathname :host nil :directory '(:relative "LOCAL")))
    (:network . ,(make-pathname :host nil :directory '(:relative "NETWORK")))
    (:user . ,(make-pathname :host nil :directory '(:relative "USER")))
    (:classic . ,(make-pathname :host nil :directory '(:relative "CLASSIC")))
    ; non-find-folder values
    (:application . ,(make-pathname :host nil :directory '(:relative "APPLICATION")))))

(defparameter *supported-framework-folder-type-logical-paths*
  `((:frameworks . ,(make-pathname :host nil :directory '(:relative "FRAMEWORKS")))
    (:resources . ,(make-pathname :host nil :directory '(:relative "RESOURCES")))))


(defun folder-attributes-to-pathname (&key (location :appropriate-disk) (folder-type :framework))
  (flet ((location-to-path (location)
           (or (rest (assoc location *supported-framework-location-logical-paths*))
               (error "invalid folder location: ~s." location)))
         (type-to-path (type)
           (or (rest (assoc type *supported-framework-folder-type-logical-paths*))
               (error "invalid folder type: ~s." type))))
    (make-pathname :host "SYS"
                   :directory `(:absolute
                                "LIBRARIES"
                                ,@(rest (pathname-directory (location-to-path location)))
                                ,@(rest (pathname-directory (type-to-path folder-type)))))))

(defgeneric folder-pathname-to-attributes (pathname)
  (:method ((pathname logical-pathname))
           (let* ((directory-path (rest (pathname-directory pathname)))
                  (location-portion (make-pathname :host nil :directory `(:relative . ,(subseq directory-path 1 2))))
                  (type-portion (make-pathname :host nil :directory `(:relative . ,(subseq directory-path 2)))))
             (flet ((path-to-location (path)
                      (or (first (rassoc path *supported-framework-location-logical-paths* :test #'equalp))
                          (error "invalid folder location: ~s." pathname)))
                    (path-to-type (path)
                      (or (first (rassoc path *supported-framework-folder-type-logical-paths* :test #'equalp))
                          (error "invalid folder type: ~s." path))))
               (values (path-to-location location-portion)
                       (path-to-type type-portion))))))

      
(defun create-application-frameworks-designator (operator &key folder-type)
  (or (let ((%bundle (#_CFBundleGetMainBundle)))
        (when (and %bundle (not (%null-ptr-p %bundle)))
          ;; in 5.1b4, this returns the root directory
          (let ((%app-directory-url (#_CFBundleCopyResourcesDirectoryURL %bundle)))
            (when (and %app-directory-url (not (%null-ptr-p %app-directory-url)))
              (unwind-protect (with-cfstrs ((%framework-directory (etypecase folder-type
                                                                    (keyword (ecase folder-type
                                                                                (:frameworks "Frameworks")
                                                                                (:resources "Resources")))
                                                                    (string folder-type))))
                                (let ((%url (#_CFURLCreateCopyAppendingPathComponent
                                             (%null-ptr)
                                             %app-directory-url
                                             %framework-directory
                                             #$true)))
                                  (when (and %url (not (%null-ptr-p %url)))
                                    (unwind-protect (rlet ((%fsref :fsref))
                                                      (cond ((#_CFURLGetFSRef %url %fsref)
                                                             (funcall operator %fsref))
                                                            (t
                                                             (error "Couldn't find application Frameworks folder: ~s."
                                                                    folder-type))))
                                      (#_CFRelease %url)))))
                (#_CFRelease %app-directory-url))))))
      (error "failed to create application URL.")))

(defun create-application-frameworks-url (&key (folder-type :frameworks))
  (create-application-frameworks-designator #'%url-from-fsref :folder-type folder-type))

(defun create-application-frameworks-pathname (&key (folder-type :frameworks))
  (create-application-frameworks-designator #'%path-from-fsref :folder-type folder-type))

(defun %url-from-fsref (fsref)
  (let* ((url (#_CFURLCreateFromFSRef (%null-ptr) fsref)))
    (if (%null-ptr-p url)
      (error "Failed to create URL")
      url)))

(defun create-frameworks-designator (operator &key (location :appropriate-disk) (folder-type :frameworks))
  (case location
    (:application ;; look for things in the application's directory tree
     (create-application-frameworks-designator operator :folder-type folder-type))
    (t
     (let (($location (or (etypecase location
                            (keyword (cdr (assoc location *supported-framework-locations*)))
                            (fixnum (when (rassoc location *supported-framework-locations* :test #'eql) location)))
                          (error "invalid framework location: ~s." location)))
           ($folder-type (or (etypecase location
                               (keyword (cdr (assoc folder-type *supported-framework-folder-types*)))
                               (fixnum (when (rassoc folder-type *supported-framework-folder-types* :test #'eql) folder-type)))
                             (error "invalid framework folder type: ~s." folder-type))))
       (rlet ((fsref :fsref))
         (let* ((err (#_FSFindFolder $location $folder-type #$true fsref)))
           (declare (type (signed-byte 16) err))
           (if (eql #$noErr err)
             (funcall operator fsref)
             (error "Couldn't find system Frameworks folder: ~s, ~s." location folder-type))))))))
#+ccl-5.2
(fmakunbound 'create-frameworks-url)

(defun create-frameworks-url (&key (location :appropriate-disk) (folder-type :frameworks))
  (create-frameworks-designator #'%url-from-fsref :location location :folder-type folder-type))

(defun create-frameworks-pathname (&key (location :appropriate-disk) (folder-type :frameworks))
  (create-frameworks-designator #'%path-from-fsref :location location :folder-type folder-type))



(defun compute-initial-framework-search-path-translations ()
  "compute and assert logical pathname translations for library directories.
   replace any existing translation for these specific pathhs, but don't 
   eliminate others."
  (flet ((compute-truename (pathname)
           "translate library logical pathname into the truename using find folder."
           (multiple-value-bind (location type)
                                (folder-pathname-to-attributes pathname)
             (create-frameworks-pathname :location location :folder-type type))))
    (mapcar #'(lambda (pathname)
                (list pathname (compute-truename pathname)))
            *initial-framework-search-path*)))
;;; (compute-initial-framework-search-path-translations)

(defun set-initial-framework-search-path-translations ()
  (let ((translations (logical-pathname-translations "SYS")))
    (loop for (logical translation) in (compute-initial-framework-search-path-translations)
          do (let ((existing (assoc logical translations :test #'equalp)))
               (cond (existing (setf (second existing) translation))
                     (t
                      (setf translations
                            (append translations (list (list logical translation))))))))
    (setf (logical-pathname-translations "SYS") translations)))

(set-initial-framework-search-path-translations)


(unless (find 'set-initial-framework-search-path-translations *lisp-startup-functions*)
  (setf *lisp-startup-functions*
        (append *lisp-startup-functions*
                '(list set-initial-framework-search-path-translations))))

(ccl::defloadvar *frameworks-url* nil)

#-ccl-5.2
(defun frameworks-url ()
  (or *frameworks-url*
      (setq *frameworks-url* (create-frameworks-url))))

#|
(mach-o-function dlopen (:string :int) :pointer "CoreFoundation.framework")
|#

(defgeneric get-framework-bundle (framework-name &key &allow-other-keys)

  (:method ((framework-name string) &key
            (if-does-not-exist :error)
            (location :appropriate-disk location-p)
            (folder-type :frameworks frameworks-p)
            (pathname (if (or location-p frameworks-p)
                        (create-frameworks-pathname :location location :folder-type folder-type)
                        *default-framework-location*)))
           "given a framework name, use either a pathname or compute one from a location/folder-type
            to construct the framework url, and load the bundle from there."
           (let ((bundle nil))
             (with-cfstrs ((%framework-name framework-name))
               (setf bundle (#_CFBundleGetBundleWithIdentifier %framework-name))
               (cond ((and bundle (not (%null-ptr-p bundle)))
                      ;; if the bundle already exists, return it
                      bundle)
                     ((and (logical-pathname-p pathname)
                           (string-equal (second (pathname-directory pathname)) "FRAMEWORKS"))
                      ;; otherwise, if it designates a framework, resolve it as such
                      (rlet ((%fsref :fsref))
                        (let* ((truename (truename pathname))
                               (framework-pathname (merge-pathnames (make-pathname :directory `(:relative ,framework-name))
                                                                    truename)))
                          (if (make-fsref-from-path-simple framework-pathname %fsref)
                            (let ((%bundle-url (#_CFURLCreateFromFSRef (%null-ptr) %fsref)))
                              (if (and %bundle-url (not (%null-ptr-p %bundle-url)))
                                (unwind-protect (let* ((%bundle (#_CFBundleCreate (%null-ptr) %bundle-url)))
                                                  (if (%null-ptr-p %bundle)
                                                    (ecase if-does-not-exist
                                                      (:error (error 'undefined-bundle :library-name framework-name :cause "Can't create bundle."))
                                                      ((nil) nil))
                                                    (cond ((#_CFBundleLoadExecutable %bundle)
                                                           (when (eq *load-verbose* 'get-framework-bundle)
                                                             (break "loaded: bundle: ~s, url:  ~s." %bundle %bundle-url)
                                                             (let* ((%bundle-path (#_CFURLGetString %bundle-url))
                                                                    (bundle-path (when (and %bundle-path (not (%null-ptr-p %bundle-path)))
                                                                                   (%get-cfstring %bundle-path))))
                                                               (format *trace-output* "~&;;; framework bundle resolves as: ~s: ~s: ~s."
                                                                       framework-name (or bundle-path "?") %bundle)))
                                                           (values %bundle :bundle))
                                                          (t
                                                           (error 'undefined-bundle :library-name framework-name
                                                                  :cause "Couldn't load bundle executable.")))))
                                  (when (and %bundle-url (not (%null-ptr-p %bundle-url)))
                                    (#_CFRelease %bundle-url)))
                                (ecase if-does-not-exist
                                  (:error (error 'undefined-bundle :library-name framework-name :cause "Can't create URL."))
                                  ((nil) nil))))
                            (ecase if-does-not-exist
                              (:error (error 'undefined-bundle :library-name framework-name :cause "Can't create FSRef."))
                              ((nil) nil))))))
                     ;; otherwise, if there is a directory try to use it as a full pathname
                     #+NYI
                     ((and (pathname-p pathname) (pathname-directory pathname))
                      (%stack-block ((%error 256))
                        (rlet ((%connID :pointer)
                               (%fsspec :fsspec)
                               (%main-address :pointer))
                          ;;; how does one
                          -> (%get-fsspec %framework %fsspec)
                          (let* ((err (#_GetDiskFragment fsspec 0 traps::$kWholeFork (%null-ptr)
                                       ;; what does this argument want
                                       ;; and where do the constants come from?
                                       (logior traps::$kLoadLib traps::$lFindLib traps::$kPowerPCArch) 
                                       %connID %mainAddr &string)))
                            (declare (type (signed-byte 16) err))
                            (if (eql #$noErr err)
                              (values (%get-ptr %connID) :disk-fragment)
                              (error "Couldn't find system Frameworks folder: ~s, ~s."
                                     framework-name (%get-cfstring %error)))))))

                     ;; otherwise, try to load it as a shared library
                     ((null pathname)
                       (values (get-shared-library framework-name) :shared-library))

                     ;; otherwise it's an error
                     (t
                      (ecase if-does-not-exist
                        (:error (error 'undefined-bundle :library-name framework-name 
                                       :cause (format nil "Can't interpret path: ~s." pathname)))
                        ((nil) nil)))))))


  (:method ((mob mach-o-bundle-description) &key (search-path (%mob-search-path mob)) (if-does-not-exist :error))
           (dolist (search-location search-path
                                    (ecase if-does-not-exist
                                      (:error (error 'undefined-bundle :library-name (%mob-library-name mob) :cause "Can't create URL."))
                                      ((nil) nil))) (print search-location)
             (multiple-value-bind (result type)
                                  (get-framework-bundle (%mob-library-name mob) :if-does-not-exist nil :pathname search-location)
               (when result (return (values result type)))))))


(defmacro with-bundle ((bundle bundle-designator &optional type-var) &rest body)
  "this excutes the body with a dynamic binding for a reference to the given bundle. the bundle executable is loaded if necessary. upon termination the bundle reference is released, bu the bundle remains resident."
  `(let* ((,bundle nil)
          ,@(when type-var `((,type-var nil))))
     (unwind-protect (progn (,@(if type-var
                                 `(multiple-value-setq (,bundle ,type-var))
                                 `(setf ,bundle))
                             (get-framework-bundle ,bundle-designator))
                            ,@body)
       (when (and ,bundle (not (%null-ptr-p ,bundle)))
         (#_CFRelease ,bundle)))))

(ccl::defloadvar *system-framework-bundle* nil)

;;; Most BSD/Mach functions are in the System framework.
#-ccl-5.2
(defun system-framework-bundle ()
  (or *system-framework-bundle*
      (setq *system-framework-bundle*
            (get-framework-bundle "System.framework"))))


(defun lookup-function-in-framework (symbol-name &optional (bundle (system-framework-bundle)))
  (with-cfstrs ((%symbol-name symbol-name))
    (let* ((addr (#_CFBundleGetFunctionPointerForName bundle %symbol-name)))
      (if (%null-ptr-p addr)
        (error "Couldn't resolve address of foreign function ~s" symbol-name)
        ;; This may be a little confusing: MCL uses fixnums (whose low 2 bits are
        ;; zero) to represent function addresses (whose low 2 bits are zero ...)
        ;; Shove the pointer in a buffer; fetch a signed 32-bit integer, shift it
        ;; right 2 bits ... voila.
        (rlet ((buf :long))
          (setf (%get-ptr buf) addr)
          (ash (%get-signed-long buf) -2))))))


;;;
;;; mach entry instances made amenable to a trap-like macro

(defparameter *type-mapping*
    '((unsigned-long :unsigned-long)
      (signed-char :signed-byte)
      (short :short)
      (double :double-float)
      (pointer :pointer)
      (handle :handle)
      (unsigned-short :unsigned-short)
      (unsigned-char :unsigned-byte)
      (unsigned-byte :unsigned-byte)
      (unsigned-int :unsigned-long :unsigned-integer)
      (signed-long :signed-long)
      (int :long :int :signed-integer)
      (fixnum :long)
      (void :void)
      (boolean :boolean)
      (string :pointer)
      (float :single-float)
      (single-float :single-float))
    "Defines a mapping from simple foreign types in Allegro Common Lisp
    to foreign types in Macintosh Common Lisp.
    the user api originated in a ff definition package for opengl, which means allegro,
    which means alternative foreign type names. this would permit including a mapping
    from apple's documented types, which differ from the 5.0 type keywords.")

(defun get-mcl-type (acl-type &optional (trap NIL))
  "Given an documented foreign type spec, returns the equivalent MCL foreign
   type spec. if its a keyword without a mapping, return it as-is.
   if it is a struct return the record type"
  ;; For specs like (pointer float) just return :pointer
  ;; For a (struct data) spec, return the record type
  (typecase acl-type
    (cons (destructuring-bind (storage data) acl-type
            (cond ((string-equal storage "pointer")
                   :pointer)
                  ((string-equal storage "struct")
                   (setf data (intern (string-upcase data) :keyword))
                   (unless (record-field-length data)
                     (warn "unknown record: ~s." data))
                   data))))
    (symbol
     ;; :void -> nil in trap defs, but not in callback defs.
     (if (and trap (string-equal acl-type "VOID"))
       nil
       (or (second (assoc acl-type *type-mapping* :test #'string-equal))
           (if (keywordp acl-type)
             acl-type
             (error "Unknown mapping for foreign type ~S." acl-type)))))))

(defun string-pointer-p (acl-type)
  (and (listp acl-type)
       (string-equal (first acl-type) "POINTER")
       (and (symbolp (second acl-type))
            (string-equal (second acl-type) "CHAR"))))





;;;
;;; bundle and entry resolution


(defun resolve-bundle-entries (mob)
  (typecase mob
    (mach-o-bundle-description
     (with-lock ((%mob-lock mob) :whostate "resolving entries")
       (let ((still-unresolved nil))
         (when *load-verbose*
           (format *trace-output* "~%;;; Loading entries from bundle ~s:" (%mob-name mob)))
         (with-bundle (cf-bundle mob type)
           (flet ((resolve-entry (key moe)
                    (declare (ignore key))
                    (ecase type
                      (:bundle
                       (when (mach-o-entry-description-p moe)
                         (unless (%moe-id moe)
                           (when *load-verbose*
                             (format *trace-output* "~%;;; resolving ~s:" (%moe-name moe)))
                           (handler-case (setf (%moe-id moe)
                                               (ccl::lookup-function-in-framework (%moe-name moe) cf-bundle))
                             ;; if the entry is not present, just note that for the subsequent warning
                             (error () (push moe still-unresolved))))))
                      (:disk-fragment )
                      (:shared-library ))))
             (declare (dynamic-extent #'resolve-entry))
             (maphash #'resolve-entry (%mob-entries mob))))
         (cond (still-unresolved
                (warn "Couldn't resolve address of entries from framework ~a:~%   ~{~<~% ~1:;~a~>~^ ~}"
                      (%mob-name mob) (sort (mapcar #'%moe-name still-unresolved) #'string<))
                nil)
               (t t)))))
    (t nil)))

(defun mach-o-bundle (interface-name &rest args &key library-name &aux mob)
  (unless (setf mob (gethash interface-name *mach-o-bundles*))
    (setf mob (apply #'make-mach-o-bundle-description :name interface-name :lock (make-lock interface-name) args))
    (setf (gethash interface-name *mach-o-bundles*) mob)
    (when *load-verbose*
      (format *trace-output* "~%;;; resolved ~a~@[[~a]~] to ~s"
              interface-name library-name (mach-o-bundle-pathname mob))))
  mob)

(defgeneric mach-o-bundle-pathname (mob)
  (:method ((mob mach-o-bundle-description))
           (with-bundle (%bundle mob)
             (let ((%executable-url (#_CFBundleCopyExecutableURL %bundle)))
               (when (and %executable-url (not (%null-ptr-p %executable-url)))
                 (unwind-protect (let ((%cfstring (#_CFURLGetString %executable-url)))
                                   (when (and %cfstring (not (%null-ptr-p %cfstring)))
                                     (ccl::%get-cfstring %cfstring)))
                   (#_CFRelease %executable-url))))))
  (:method ((name string))
           (let ((mob (mach-o-bundle name)))
             (if mob
               (mach-o-bundle-pathname mob)
               nil))))
;;; (mach-o-bundle-pathname "Oracle.framework")
;;; (mach-o-bundle-pathname "LIBFFTW3.framework")
;;; (clrhash *mach-o-bundles*)

(defun list-all-bundles ()
  (let ((result nil))
    (maphash #'(lambda (k v) (declare (ignore v)) (push k result)) *mach-o-bundles*)
    result))
;;(list-all-bundles)
;;(resolve-bundle-entries (mach-o-bundle "Oracle.framework"))
;;(resolve-bundle-entries (mach-o-bundle "SSL.framework"))
;;(resolve-bundle-entries (mach-o-bundle "LIBFFTW3.framework"))
;;(resolve-bundle-entries (mach-o-bundle "ApplicationServices.framework"))

(defun mach-o-entry (trap-name entry-name library-name &key (if-exists t) (if-does-not-exist :create) &aux moe mob)
  "locate an existing entry description, or generate a new one, and possibly a new bundle description. observe if-exists and if-does-not-exist specifications. nb. this is not thread-safe: if one ever wants to do parallel compilation/loading it will need to lock the table and then the mob"
  (setf mob (mach-o-bundle library-name))
  (if (setf moe (gethash trap-name (%mob-entries mob)))
    (ecase if-exists
      ((t) (values moe t))
      (:clear (setf (%moe-id moe) nil)
              (unless (equal (%moe-name moe) entry-name)
                (setf (%moe-name moe) entry-name))
              (values moe t))
      (:delete (remhash trap-name (%mob-entries mob))
               (values moe t))
      (:error (error "mach entry already exists: ~s." trap-name))
      ((nil) (values nil t)))
    (ecase if-does-not-exist
      (:create (setf moe (make-mach-o-entry-description :name entry-name :bundle mob :trap-name trap-name))
               (setf (gethash trap-name (%mob-entries mob)) moe)
               (values moe nil))
      (:error (error "no registered mach entry: ~s." trap-name))
      ((nil) nil))))

(defun clear-mach-o-entries ()
  (maphash #'(lambda (key mob) (declare (ignore key))
              (maphash #'(lambda (key moe)
                           (declare (ignore key))
                           (when (mach-o-entry-description-p moe)
                             (setf (%moe-id moe) nil)))
                       (%mob-entries mob)))
           *mach-o-bundles*))

(pushnew 'clear-mach-o-entries *lisp-cleanup-functions*)


(defun ppc-macho-keywordized-arglist (passed-args inline-args)
  (labels ((struct-length (name)
             (logand (1+ (record-field-length (intern (string-upcase name) :keyword))) #xffffffe))
           (expanded-argument (passed type &aux mac-type)
             (if (consp type)
               (expanded-argument passed (first type))          ; pointer, handle
               (if (setf mac-type (find-mactype type nil))
                 (list (mactype=>ppc-ff-call-type mac-type) passed)
                 (let ((length (struct-length type))
                       (references nil)
                       (offset 0))
                   (unless (zerop (mod length 4))
                     (warn "!? padding length to fullword multiple: ~s: ~s." type length)
                     (incf length (- 4 (mod length 4))))
                   (loop (when (>= offset length) (return))
                         (push :unsigned-fullword references)
                         (push (if passed `(%get-unsigned-long ,passed ,offset) 0) references)
                         (incf offset 4))
                   (reverse references))))))
    (loop for (nil type) in inline-args
          for passed in passed-args
          append (expanded-argument passed type))))

(defun ppc-macho-ff-assignment (sp-binding rv-binding return-type)
  (let ((length (logand (1+ (record-field-length (intern (string-upcase return-type) :keyword))) #xffffffe)))
    `(print '(dotimes (i ,length)
               (setf (%get-unsigned-byte ,rv-binding i) (%get-unsigned-long ,sp-binding i))))))

(defun macho-type-coerced-args (args inline-args c?)
  (loop for arg in args
        for (nil rawtype) in inline-args
        for type = (find-mactype rawtype nil)
        collect 
        (if (and type (eql (mactype-record-size type) 1))
          (case (mactype-name type)
            (:unsigned-byte (if c? arg `(ash (logand #xff ,arg) 8)))
            (:boolean `(if ,arg -1 0))          ; #$true #$false
            (:signed-byte (if c? arg
                              (let ((temp (gensym)))
                                `(let ((,temp (ldb (byte 8 0) ,arg)))
                                   (if (> ,temp 128) 
                                     (- (ash ,temp 8) #xffff)
                                     (ash ,temp 8))))))
            (:character (if c? `(char-code ,arg) `(ash (char-code ,arg) 8)))
            (otherwise arg))
          arg)))

(defun expand-mach-o-trap (name passed-args inline-args return-type &optional can-errcheck?
                                entry-name library-name env-var return-var
                                &aux do-memerr?)
  "generate a ppc-ff-call form which extracts the address from a mach-o-entry instance.
   the instance is generated at load-time, but the address is resolved and cached upon first reference.
   if an atomic value is returned, coerced as necessary.
   if a record is returned by value (return is non-null) then extend the argument list and copy the value off the stack.
   both the args and the return type are supplied as mac-type keywords, that is primitive types, pointer, handle, or record
   keynames."
  
  (let* ((c-type-coercion? t)
         (ppc-target-p  (ppc-target-p)))
    (unless ppc-target-p
      (format t "~&;; Warning: Trap ~a has no defined implementation~&" name)
      (return-from expand-mach-o-trap `(error "Trap ~a has no defined-implementation" ',name)))
    
    (when (member :no-errchk passed-args)
      (setq passed-args (remove :no-errchk passed-args)))
    
    (when (member-if #'(lambda (arg)
                         (memq arg *error-check-keywords*))
                     passed-args)
      (setq passed-args (remove-if #'(lambda (arg) (memq arg *error-check-keywords*)) passed-args))
      (setq do-memerr? t))
    
    (when (and (not can-errcheck?) do-memerr?)
      (format t "~&;; :errchk keyword wrongly found in ~a. Ignoring.~&" name))
    
    (let ((passed-length (length passed-args))
          (inline-length (length inline-args)))
      (declare (fixnum passed-length inline-length))
      (when (> passed-length inline-length)
        (error "Too many args in trap reference:~%~a" `(,name ,@passed-args)))
      (when (< passed-length inline-length)
        (error "Too few args in trap reference:~%~a" `(,name ,@passed-args))))
    
    (let* ((constants-checked
            (mapcar #'(lambda (arg trap-arg &aux ct-check)
                        (if (and (constantp arg)
                                 (setq ct-check
                                       (mactype-ct-type-check 
                                        (find-arg-mactype (cadr trap-arg)))))
                          (if (funcall ct-check (eval arg))
                            t
                            (error "Argument ~s (~s) to trap ~a is not of type ~s"
                                   arg (car trap-arg) name (cadr trap-arg)))
                          nil))
                    passed-args inline-args))
           (arg-vars (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) passed-args))
           (rt-type-check-code (expand-trap-rt-type-check constants-checked
                                                          (mapcar #'(lambda (var trap-arg)
                                                                      (cons var (cadr trap-arg)))
                                                                  arg-vars inline-args)
                                                          env-var))
           (inside-args (if rt-type-check-code arg-vars passed-args))
           (type-coerced-args (macho-type-coerced-args inside-args inline-args c-type-coercion?))
           (form nil)
           (mac-return-type (and return-type (find-mactype return-type nil))))
      (cond (return-var
             ;; a non-primitive, record type is supplied as the return type
             (setf form `(ppc-ff-call (or (%moe-id (load-time-value (mach-o-entry ',name ,entry-name ,library-name)))
                                          (mach-o-entry-id (load-time-value (mach-o-entry ',name ,entry-name ,library-name))))
                                      ,@(ppc-macho-keywordized-arglist (list return-var) `((,return-var :pointer)))
                                      ,@(ppc-macho-keywordized-arglist type-coerced-args inline-args)
                                      :void)))
            (mac-return-type
             ;; a primitive, atomic type
             (setf form `(ppc-ff-call (or (%moe-id (load-time-value (mach-o-entry ',name ,entry-name ,library-name)))
                                          (mach-o-entry-id (load-time-value (mach-o-entry ',name ,entry-name ,library-name))))
                                      ,@(ppc-macho-keywordized-arglist type-coerced-args inline-args)
                                      ,(mactype=>ppc-ff-call-type mac-return-type)))
             (setq form
                   (if c-type-coercion? 
                     (coerced-c-result form return-type ppc-target-p)
                     (coerced-pascal-result form return-type))))
            (t
             (setf form `(ppc-ff-call (or (%moe-id (load-time-value (mach-o-entry ',name ,entry-name ,library-name)))
                                          (mach-o-entry-id (load-time-value (mach-o-entry ',name ,entry-name ,library-name))))
                                      ,@(ppc-macho-keywordized-arglist type-coerced-args inline-args)
                                      :void))))
      
      (when rt-type-check-code
        (setq form `(let ,(mapcar 'list arg-vars passed-args)
                      (declare (dynamic-extent ,@arg-vars))
                      ,@rt-type-check-code ,form)))
      (if do-memerr?
        (cond ((memq name *memerror-traps*)
               `(memerror-check ,form))
              ((memq name *reserror-traps*)
               `(reserror-check ,form))
              (t `(errchk ,form)))
        form))))

(defMacro %setr (record ff-call-form)
  (append ff-call-form (list record)))



(defmacro def-mach-o-trap (trap-name entry-name library-name args return-type &key allow-errchk?)
  (dolist (argspec args)
    (when (memq (car argspec) *variable-names-macro-arglists-shouldnt-use*)
      (setf (car argspec) (bad-variable-name-replacement (car argspec)))))
  (let ((env-var (gensym))
        (return-var (and return-type (not (string-equal return-type :void)) (not (find-mactype return-type nil)) (gensym "RETURN-"))))
    `(progn
       (setf (gethash ',trap-name %trap-strings%) (string ',trap-name))
       (mach-o-entry ',trap-name ,entry-name ,library-name :if-exists :clear)
       (defmacro ,trap-name (&environment ,env-var ,@(mapcar 'car args)
                                          ,@(if return-var (list return-var) nil)
                                          ,@(if allow-errchk? '(&optional (errchk? :no-errchk)) nil))
         (declare (ignore-if-unused errchk?))
         (expand-mach-o-trap ',trap-name
                             (list ,@(if allow-errchk? (list 'errchk?) nil) ,@(mapcar 'car args))
                             ',args ',return-type ,allow-errchk? ,entry-name ,library-name ,env-var 
                             ,(if return-var return-var nil))))))

(defmacro require-mach-o-trap (trap-name &rest rest)
  ;; just a place-holder in case one wants to include autoloading
  `(,trap-name ,@rest))


(defmacro mach-o-function (name arguments return-type library#entry)
  (let* ((dot (position #\# library#entry :from-end t))
         (library-name (if dot (subseq library#entry 0 dot) "System.framework"))
         (entry-name (if dot (subseq library#entry (1+ dot)) library#entry))
         (trap-name (intern (concatenate 'string "_" (string name)) :traps))
         (return-record? (and (consp return-type) (string-equal "struct" (first return-type)))))
    (let* ((args (let ((counter 0))
                   (mapcar #'(lambda (arg)
                               (declare (ignore arg))
                               (intern (format nil "ARG~S" (incf counter))))
                           arguments)))
           (extended-args (if return-record? (append args '(return-value)) args))
           (arg-mcl-types (mapcar #'get-mcl-type arguments))
           (return-mcl-type (get-mcl-type return-type))
           (extended-arg-mcl-types (if return-record? (append arg-mcl-types (list return-mcl-type)) arg-mcl-types))
           (arg-string-vars (mapcar #'(lambda (arg)
                                        (if (string-pointer-p arg)
                                          (gensym)
                                          nil))
                                    (if return-record? (append arguments (list return-type)) arguments))))
      ;; construct trap macro and function given the trap/function name, entry name, and library name, and the arguments.
      ;; the trap definition macro is invoked with argument and return type specifications of the supplied, but mapped to mcl
      ;; types, which means atomic primitive types and pointer or handle references are identified as such, which
      ;; records - specified as (struct <record namestring>), are identified by record keyword
      ;; the function definition uses the possibly extended argument list inorder to accommodate the possible return value
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (def-mach-o-trap ,trap-name ,entry-name ,library-name
           ,(mapcar #'(lambda (name type)
                        `(,name ,type))
                    args
                    arg-mcl-types)
           ,(get-mcl-type return-type T))
         (defun ,(intern (string-upcase entry-name)) ,extended-args
           ,(let ((call `(require-mach-o-trap ,trap-name
                                              ,@(mapcar #'(lambda (name type string-var)
                                                            (cond ((eq type :single-float)
                                                                   `(float ,name 0.0s0))
                                                                  (string-var
                                                                   string-var)
                                                                  (T
                                                                   name)))
                                                        extended-args
                                                        extended-arg-mcl-types
                                                        arg-string-vars))))
              (if (some #'identity arg-string-vars)
                `(ccl:with-cstrs ,(mapcan #'(lambda (gvar var)
                                              (if gvar
                                                (list (list gvar var))
                                                nil))
                                          arg-string-vars
                                          args)
                   ,call)
                call)))
         (export ',name)
         ',name))))


(defGeneric describe-mob (mob stream)
  (:method ((name string) stream &aux mob)
           (if (setf mob (gethash name *mach-o-bundles*))
             (describe-mob mob stream)
             (warn "no mach-o bundle desription: ~s" name)))
  (:method ((mob mach-o-bundle-description) stream)
           (fresh-line stream)
           (describe mob stream)
           (maphash #'(lambda (key moe)
                        (declare (ignore key))
                        (print moe stream))
                    (%mob-entries mob))))



;;;
;;; translate inline-trap definitions from the exisiting interface files
;;;
;;; these files include defconstant, def-mactype, defrecord, and deftrap-inline forms,
;;; and are linked by require-interface forms. only the deftrap-inline need be
;;; translated and the require stipulations are used to infer dependancies are
;;; translate the entire group.
;;;
;;; the result file has the translated entry definitions only.
;;;

(defParameter *library-name* nil)
(defParameter *from* nil)
(defParameter *to* nil)
(defParameter *start* nil)
(defparameter *entries-only* t)

(defun copy-line-comment (from to)
  (let ((char #\space))
    (loop (setf char (read-char from nil :eof))
          (unless *entries-only* (write-char char to))
          (case char
            ((#\return #\linefeed :eof)
             (return))
            (t
             )))))

(defun copy-block-comment (from to &aux char)
  (setf char (read-char from))
  (unless *entries-only*  (write-char char to))
  (when (eql #\| (peek-char nil from nil :eof))
    (setf char (read-char from))
    (unless *entries-only*  (write-char char to))
    (loop (case (setf char (read-char from nil :eof))
            (:eof (return))
            (#\#
             (unread-char #\# from)
             (copy-block-comment from to))
            (#\|
             (unless *entries-only* (write-char char to))
             (case (peek-char nil from nil :eof)
               (#\#
                (setf char (read-char from))
                (unless *entries-only* (write-char char to))
                (return))
               (:eof (return))
               (t )))
            (t
             (unless *entries-only* (write-char char to)))))))

(defGeneric translate-interface-file (from to &key &allow-other-keys)
  (:method ((from t) (to t) &rest args)
           (format *trace-output* "~%;;; translating: ~s -> ~s" from to)
           (with-open-file (from-stream from :direction :input)
             (apply #'translate-interface-file from-stream to args)))
  (:method ((from-stream stream) (to t) &rest args &key (if-exists :supersede) (if-does-not-exist :create))
           (with-open-file (to-stream to :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
             (apply #'translate-interface-file from-stream to-stream args)))
  (:method ((*from* stream) (*to* stream) &key ((:library-name *library-name*) *library-name*) &allow-other-keys
            &aux (*start* (or *start* (get-universal-time))) (eof (gensym)) (char #\space))
           (format *to* ";;;~%;;; generated from ~s." (namestring (pathname *from*)))
           (let ((form nil))
             (loop
               (case (peek-char nil *from* nil :eof)
                 (:eof (return))
                 (#\# (copy-block-comment *from* *to*))
                 (#\; (copy-line-comment *from* *to*))
                 ((#\return #\linefeed #\space #\tab)
                  (setf char (read-char *from*))
                  (unless *entries-only* (write-char char *to*)))
                 (t (if (eq eof (setf form (read-preserving-whitespace *from* nil eof)))
                      (return)
                      (typecase form
                        (cons (translate-interface-form form *to*))
                        (t (unless *entries-only* (format *to* "~s" form)))))))))))

(defun translate-interface-form (form  to)
  (labels ((translate-parameter (arg)
             (destructuring-bind (name type) arg
               (declare (ignore name))
               (translate-type type)))
           (translate-type (type)
             ;; rebind the readtable, as looking for the record definition may cause the trap file to be loaded
             (cond ((consp type)
                    (cond ((equalp '(:string 255) type)
                           `(pointer char))
                          ((eq (first type) :pointer)
                           `(pointer ,(translate-type (second type))))
                          (t
                           (warn "can't translate type: ~s." type)
                           type))) ; `(pointer ,(translate-type type))
                    ((string-equal type "nil")
                     'void)
                    ((first (find-if #'(lambda (options) (find type options :test #'string-equal))
                                     *type-mapping* :key #'rest)))
                     ;; this requires that the existing preliminary interface files are loadable
                    ((record-info type)
                     `(struct ,(string type)))
                    (t
                     (warn "can't translate type: ~s." type)
                     type))))
    (case (intern (string-upcase (first form)) :keyword)
      (:defrecord (format to "~%(defrecord ~{~s~^ ~})" (rest form)))
      (:deftrap-inline (destructuring-bind (op name args return inlines &rest keys) form
                         (declare (ignore op))
                         (when (or inlines keys)
                           (format to "~%;;; ignoring inlines and/or keys: ~s" form))
                         (format to "~2%(mach-o-function ~a~%~17T~:@(~s~)~%~17T~s~%~17T\"~@[~a#~]~a\")"
                                 (subseq name 1)
                                 (mapcar #'translate-parameter args)
                                 (translate-type return)
                                 *library-name*
                                 (subseq name 1))))
      (:provide-interface (unless *entries-only* (format to "~%;;; ignoring : ~s" form)))
      (:require-interface (destructuring-bind (require (quote lib)) form
                            (declare (ignore require quote))
                            (let ((required-from (make-pathname :name (string lib) :defaults *from*))
                                  (required-to (make-pathname :name (concatenate 'string (string lib) "-mach-o")
                                                              :defaults *to*)))
                              (format to "~%;;; dependant on : ~s" (namestring required-from))
                              (when (or (null (probe-file required-to))
                                        (< (file-write-date required-to) *start*))
                                (translate-interface-file required-from required-to)))))
      (:in-package (format to "~2%~s" form))
      (t (unless *entries-only*  (format to "~s" form))))))


(pushnew :mach-o-traps *features*)

#|
(mapcar #'second *type-mapping*)
(clrhash *mach-o-bundles*)
(resolve-bundle-entries (gethash "ApplicationServices.framework" *mach-o-bundles*))
(describe-mob "ApplicationServices.framework" *trace-output*)

(translate-interface-file
 "osx.tschichold:Applications:MCL:MCL 5.0:Library:interfaces:CGContext.lisp"
 "yoda:Development:Source:dev:de:setf:object-graphics:code:interfaces2:CGContext-mach-o.lisp"
 :if-exists :supersede
 :library-name "ApplicationServices.framework")


(trace translate-interface-file)
(rlet ((point :cgpoint))
  (#_CGPointMake 0.0s0 0.0s0 point)
  (print point)
  (values))
(untrace)

(#_CGAffineTransformMake 1.0s0 2.0s0 3.0s0 4.0s0 5.0s0 6.0s0)

|#


:EOF
