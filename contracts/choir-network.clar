;; Therapeutic Community Choir Network - Core Contract
;; A decentralized platform for healing singing groups with comprehensive tracking and coordination

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_INPUT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_SESSION_FULL (err u105))
(define-constant ERR_PAST_DATE (err u106))

;; Data Variables
(define-data-var contract-uri (optional (string-utf8 256)) none)
(define-data-var platform-fee uint u50) ;; 0.5% fee in basis points
(define-data-var next-choir-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var next-song-id uint u1)

;; Data Maps
(define-map choirs
  uint
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    facilitator: principal,
    created-at: uint,
    therapeutic-focus: (string-utf8 50), ;; anxiety, depression, trauma, general-wellness
    max-participants: uint,
    current-participants: uint,
    cultural-tradition: (optional (string-utf8 50)),
    accessibility-features: (list 5 (string-utf8 30)), ;; sign-language, large-print, wheelchair, hearing-aid, etc.
    is-active: bool
  }
)

(define-map choir-participants
  {choir-id: uint, participant: principal}
  {
    joined-at: uint,
    voice-type: (string-utf8 20), ;; soprano, alto, tenor, bass, unknown
    accessibility-needs: (list 3 (string-utf8 30)),
    therapeutic-goals: (list 5 (string-utf8 50)),
    participation-level: uint ;; 1-5 scale
  }
)

(define-map therapy-sessions
  uint
  {
    choir-id: uint,
    session-date: uint,
    duration-minutes: uint,
    theme: (string-utf8 100),
    facilitator: principal,
    max-participants: uint,
    registered-count: uint,
    songs-performed: (list 10 uint), ;; list of song-ids
    therapeutic-techniques: (list 5 (string-utf8 50)), ;; breathing, visualization, movement, etc.
    emotional-check-in: bool,
    is-completed: bool
  }
)

(define-map session-registrations
  {session-id: uint, participant: principal}
  {
    registered-at: uint,
    pre-session-mood: (optional uint), ;; 1-10 scale
    post-session-mood: (optional uint),
    voice-progress-notes: (optional (string-utf8 200)),
    emotional-insights: (optional (string-utf8 300))
  }
)

(define-map cultural-songs
  uint
  {
    title: (string-utf8 100),
    cultural-origin: (string-utf8 50),
    language: (string-utf8 30),
    therapeutic-properties: (list 5 (string-utf8 30)), ;; calming, energizing, grounding, etc.
    difficulty-level: uint, ;; 1-5 scale
    contributor: principal,
    preservation-notes: (optional (string-utf8 300)),
    audio-reference: (optional (string-utf8 200)), ;; IPFS hash or URL
    is-approved: bool
  }
)

(define-map therapeutic-outcomes
  {participant: principal, timeframe: uint} ;; timeframe as month/year combo
  {
    sessions-attended: uint,
    average-pre-mood: uint,
    average-post-mood: uint,
    voice-development-score: uint, ;; 1-100 scale
    community-connection-score: uint, ;; 1-100 scale
    self-reported-wellbeing: uint, ;; 1-10 scale
    goals-achieved: (list 5 (string-utf8 50))
  }
)

(define-map facilitator-credentials
  principal
  {
    certified: bool,
    specializations: (list 5 (string-utf8 50)),
    experience-years: uint,
    certification-expiry: uint,
    community-rating: uint ;; 1-100 scale
  }
)

;; Helper Functions
(define-private (is-facilitator-certified (facilitator principal))
  (match (map-get? facilitator-credentials facilitator)
    credentials (and (get certified credentials) (> (get certification-expiry credentials) stacks-block-height))
    false
  )
)

(define-private (calculate-mood-improvement (pre-mood uint) (post-mood uint))
  (if (> post-mood pre-mood)
    (- post-mood pre-mood)
    u0
  )
)

(define-private (validate-accessibility-features (features (list 5 (string-utf8 30))))
  (let ((valid-features (list "sign-language" "large-print" "wheelchair-accessible"
                             "hearing-aid-compatible" "visual-cues" "quiet-space"
                             "flexible-seating" "interpreter-services")))
    ;; In production, would validate each feature against the valid list
    true
  )
)

;; Public Functions - Choir Management
(define-public (create-choir
  (name (string-utf8 100))
  (description (string-utf8 500))
  (therapeutic-focus (string-utf8 50))
  (max-participants uint)
  (cultural-tradition (optional (string-utf8 50)))
  (accessibility-features (list 5 (string-utf8 30)))
)
  (let ((choir-id (var-get next-choir-id)))
    (asserts! (is-facilitator-certified tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> max-participants u0) ERR_INVALID_INPUT)
    (asserts! (validate-accessibility-features accessibility-features) ERR_INVALID_INPUT)

    (map-set choirs choir-id {
      name: name,
      description: description,
      facilitator: tx-sender,
      created-at: stacks-block-height,
      therapeutic-focus: therapeutic-focus,
      max-participants: max-participants,
      current-participants: u0,
      cultural-tradition: cultural-tradition,
      accessibility-features: accessibility-features,
      is-active: true
    })

    (var-set next-choir-id (+ choir-id u1))
    (ok choir-id)
  )
)

(define-public (join-choir
  (choir-id uint)
  (voice-type (string-utf8 20))
  (accessibility-needs (list 3 (string-utf8 30)))
  (therapeutic-goals (list 5 (string-utf8 50)))
)
  (let ((choir-data (unwrap! (map-get? choirs choir-id) ERR_NOT_FOUND)))
    (asserts! (get is-active choir-data) ERR_NOT_FOUND)
    (asserts! (< (get current-participants choir-data) (get max-participants choir-data)) ERR_SESSION_FULL)
    (asserts! (is-none (map-get? choir-participants {choir-id: choir-id, participant: tx-sender})) ERR_ALREADY_EXISTS)

    (map-set choir-participants
      {choir-id: choir-id, participant: tx-sender}
      {
        joined-at: stacks-block-height,
        voice-type: voice-type,
        accessibility-needs: accessibility-needs,
        therapeutic-goals: therapeutic-goals,
        participation-level: u1
      }
    )

    (map-set choirs choir-id
      (merge choir-data {current-participants: (+ (get current-participants choir-data) u1)})
    )

    (ok true)
  )
)

;; Public Functions - Session Management
(define-public (schedule-therapy-session
  (choir-id uint)
  (session-date uint)
  (duration-minutes uint)
  (theme (string-utf8 100))
  (max-participants uint)
  (therapeutic-techniques (list 5 (string-utf8 50)))
)
  (let ((session-id (var-get next-session-id))
        (choir-data (unwrap! (map-get? choirs choir-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get facilitator choir-data)) ERR_UNAUTHORIZED)
    (asserts! (> session-date stacks-block-height) ERR_PAST_DATE)
    (asserts! (> duration-minutes u0) ERR_INVALID_INPUT)

    (map-set therapy-sessions session-id {
      choir-id: choir-id,
      session-date: session-date,
      duration-minutes: duration-minutes,
      theme: theme,
      facilitator: tx-sender,
      max-participants: max-participants,
      registered-count: u0,
      songs-performed: (list),
      therapeutic-techniques: therapeutic-techniques,
      emotional-check-in: true,
      is-completed: false
    })

    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (register-for-session (session-id uint))
  (let ((session-data (unwrap! (map-get? therapy-sessions session-id) ERR_NOT_FOUND)))
    (asserts! (< (get registered-count session-data) (get max-participants session-data)) ERR_SESSION_FULL)
    (asserts! (is-none (map-get? session-registrations {session-id: session-id, participant: tx-sender})) ERR_ALREADY_EXISTS)

    ;; Verify participant is in the choir
    (asserts! (is-some (map-get? choir-participants {choir-id: (get choir-id session-data), participant: tx-sender})) ERR_UNAUTHORIZED)

    (map-set session-registrations
      {session-id: session-id, participant: tx-sender}
      {
        registered-at: stacks-block-height,
        pre-session-mood: none,
        post-session-mood: none,
        voice-progress-notes: none,
        emotional-insights: none
      }
    )

    (map-set therapy-sessions session-id
      (merge session-data {registered-count: (+ (get registered-count session-data) u1)})
    )

    (ok true)
  )
)

(define-public (record-session-mood
  (session-id uint)
  (pre-mood (optional uint))
  (post-mood (optional uint))
  (voice-notes (optional (string-utf8 200)))
  (emotional-insights (optional (string-utf8 300)))
)
  (let ((registration-key {session-id: session-id, participant: tx-sender})
        (current-registration (unwrap! (map-get? session-registrations registration-key) ERR_NOT_FOUND)))

    ;; Validate mood scores are 1-10
    (match pre-mood mood (asserts! (and (>= mood u1) (<= mood u10)) ERR_INVALID_INPUT) true)
    (match post-mood mood (asserts! (and (>= mood u1) (<= mood u10)) ERR_INVALID_INPUT) true)

    (map-set session-registrations registration-key
      (merge current-registration {
        pre-session-mood: pre-mood,
        post-session-mood: post-mood,
        voice-progress-notes: voice-notes,
        emotional-insights: emotional-insights
      })
    )

    (ok true)
  )
)

;; Public Functions - Cultural Song Preservation
(define-public (contribute-cultural-song
  (title (string-utf8 100))
  (cultural-origin (string-utf8 50))
  (language (string-utf8 30))
  (therapeutic-properties (list 5 (string-utf8 30)))
  (difficulty-level uint)
  (preservation-notes (optional (string-utf8 300)))
  (audio-reference (optional (string-utf8 200)))
)
  (let ((song-id (var-get next-song-id)))
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR_INVALID_INPUT)

    (map-set cultural-songs song-id {
      title: title,
      cultural-origin: cultural-origin,
      language: language,
      therapeutic-properties: therapeutic-properties,
      difficulty-level: difficulty-level,
      contributor: tx-sender,
      preservation-notes: preservation-notes,
      audio-reference: audio-reference,
      is-approved: false
    })

    (var-set next-song-id (+ song-id u1))
    (ok song-id)
  )
)

(define-public (approve-cultural-song (song-id uint))
  (let ((song-data (unwrap! (map-get? cultural-songs song-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    (map-set cultural-songs song-id
      (merge song-data {is-approved: true})
    )

    (ok true)
  )
)

;; Public Functions - Facilitator Management
(define-public (register-facilitator
  (specializations (list 5 (string-utf8 50)))
  (experience-years uint)
  (certification-expiry uint))
  (begin
    (asserts! (> certification-expiry stacks-block-height) ERR_INVALID_INPUT)

    (map-set facilitator-credentials tx-sender {
      certified: false, ;; Requires admin approval
      specializations: specializations,
      experience-years: experience-years,
      certification-expiry: certification-expiry,
      community-rating: u50
    })

    (ok true)
  )
)

(define-public (certify-facilitator (facilitator principal))
  (let ((credentials (unwrap! (map-get? facilitator-credentials facilitator) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    (map-set facilitator-credentials facilitator
      (merge credentials {certified: true})
    )

    (ok true)
  )
)

;; Public Functions - Outcome Tracking
(define-public (update-therapeutic-outcomes
  (timeframe uint) ;; YYYYMM format
  (sessions-attended uint)
  (voice-development-score uint)
  (community-connection-score uint)
  (self-reported-wellbeing uint)
  (goals-achieved (list 5 (string-utf8 50)))
)
  (let ((outcome-key {participant: tx-sender, timeframe: timeframe}))
    (asserts! (and (>= voice-development-score u1) (<= voice-development-score u100)) ERR_INVALID_INPUT)
    (asserts! (and (>= community-connection-score u1) (<= community-connection-score u100)) ERR_INVALID_INPUT)
    (asserts! (and (>= self-reported-wellbeing u1) (<= self-reported-wellbeing u10)) ERR_INVALID_INPUT)

    ;; Calculate average moods from recent sessions (simplified for demo)
    (map-set therapeutic-outcomes outcome-key {
      sessions-attended: sessions-attended,
      average-pre-mood: u5, ;; Would calculate from actual data
      average-post-mood: u7, ;; Would calculate from actual data
      voice-development-score: voice-development-score,
      community-connection-score: community-connection-score,
      self-reported-wellbeing: self-reported-wellbeing,
      goals-achieved: goals-achieved
    })

    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-choir (choir-id uint))
  (map-get? choirs choir-id)
)

(define-read-only (get-session (session-id uint))
  (map-get? therapy-sessions session-id)
)

(define-read-only (get-cultural-song (song-id uint))
  (map-get? cultural-songs song-id)
)

(define-read-only (get-participant-info (choir-id uint) (participant principal))
  (map-get? choir-participants {choir-id: choir-id, participant: participant})
)

(define-read-only (get-therapeutic-outcomes (participant principal) (timeframe uint))
  (map-get? therapeutic-outcomes {participant: participant, timeframe: timeframe})
)

(define-read-only (get-facilitator-credentials (facilitator principal))
  (map-get? facilitator-credentials facilitator)
)

(define-read-only (is-registered-for-session (session-id uint) (participant principal))
  (is-some (map-get? session-registrations {session-id: session-id, participant: participant}))
)

(define-read-only (get-session-registration (session-id uint) (participant principal))
  (map-get? session-registrations {session-id: session-id, participant: participant})
)

;; Admin Functions
(define-public (initialize-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set facilitator-credentials CONTRACT_OWNER {
      certified: true,
      specializations: (list (u"trauma-informed-care") (u"music-therapy") (u"community-healing") (u"vocal-development") (u"group-dynamics")),
      experience-years: u10,
      certification-expiry: (+ stacks-block-height u52560), ;; ~1 year
      community-rating: u100
    })
    (ok true)))

(define-public (set-contract-uri (uri (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-uri (some uri))
    (ok true)
  )
)

(define-public (set-platform-fee (fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= fee u1000) ERR_INVALID_INPUT) ;; Max 10%
    (var-set platform-fee fee)
    (ok true)
  )
)

;; Contract initialization will be done through deployment script or first function call
