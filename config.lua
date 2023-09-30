-- This file contains all the user configurable options
-- Entries which aren't marked as REQUIRED can be ommitted completely
local Config = {
    ----------------------------------------------
    ---- [[ GENERAL CONFIGURATION OPTIONS ]] -----
    ----------------------------------------------
    -- This refers to the IP address of the PC ankiconnect is running on
    -- Remember to expose the port ankiconnect listens on so we can connect to it
    -- [REQUIRED} The ankiconnect settings also need to be updated to not only listen on the loopback address
    url = "http://localhost:8765",
    -- [REQUIRED} name of the anki deck
    deckName = "日本::3 - Mining Deck",
    -- [REQUIRED} note type of the notes that should be created
    modelName = "Japanese sentences",
    -- Each note created by the plugin will have the tag 'KOReader', it is possible to add other custom tags
    -- A card with custom tags can be created by pressing and holding the 'Add to Anki' button, which pops up a menu with some extra options.
    custom_tags = { "NEEDS_WORK" },

    -- It is possible to toggle whether duplicate notes can be created. This can be of use if your note type contains the full sentence as first field (meaning this gets looked at for uniqueness)
    -- When multiple unknown words are present, it won't be possible to add both in this case, because the sentence would be the same.
    allow_dupes = false,
    -- The scope where ankiconnect will look to to find duplicates
    dupe_scope = "deck",

    -- Close dictionary window after adding a new card
    -- Note: I don't know why, but if you set this to true, the corresponding menu item will remain stuck activated
    close_dictionary = true,

    --------------------------------------------------
    --- [[ CUSTOM CONTEXT CONFIGURATION OPTIONS ]] ---
    --------------------------------------------------
    -- Tapping the 'Add to Anki' button will always open the custom context window if this is active
    -- Otherwise, pressing this button will immediately create a new card with the default context.
    -- Note: I don't know why, but if you set this to true, the corresponding menu item will remain stuck activated
    always_open_custom_context = true,
    -- How many chars holding the ±1 buttons will jump inside the custom context window
    custom_context_jump_size = 8,
    -- Punctuation that is used to split sentences / sentence fragments (    buttons, also determines default context)
    fragment_delimiters = "「」『』（）【】、。！？().?!,\"'",
    -- Which trailing delimiters should be retained at the end of the context
    retained_trailing_delimiters = "、。！？.?!,",


    ----------------------------------------------
    --- [[ NOTE FIELD CONFIGURATION OPTIONS ]] ---
    ----------------------------------------------
    -- [REQUIRED] The field name where the word which was looked up in a dictionary will be sent to.
    word_field = "VocabKanji",

    -- The field name where the sentence in which the word we looked up occurred will be sent to.
    context_field = "Sentence-Kanji",

    -- [REQUIRED] The field name where the dictionary definition will be sent to.
    def_field = "VocabDef",

    -- The field name where metadata (book source, page number, ...) will be sent to.
    -- This metadata is parsed from the EPUB's metadata, or from the filename
    meta_field = "Notes",

    -- The plugin can query Forvo for audio of the word you just looked up.
    -- The field name where the audio will be sent to.
    audio_field = "Vocabulary-Audio",

    -- list of extensions which should be enabled, by default they are all off
    -- an extension is turned on by listing its filename in the table below
    enabled_extensions = {
        --[[
        "EXT_dict_edit.lua",
        "EXT_dict_word_lookup.lua",
        "EXT_multi_def.lua",
        "EXT_pitch_accent.lua"
        --]]
    }
}
return Config
