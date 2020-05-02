// This is free and unencumbered software released into the public domain.

function notify(msg, options)
{
    if (msg == null) {throw 'Argument "msg" is required';}

    let app  = Application.currentApplication();
    let args = {};

    app.includeStandardAdditions = true;

    if (typeof options === 'object' && options !== 'null') {
        if ('title' in options) {
            args['withTitle'] = options.title;
        }

        if ('subtitle' in options) {
            args['subtitle']  = options.subtitle;
        }

        if ('sound' in options) {
            args['soundName'] = options.sound;
        }
    }

    app.displayNotification(msg, args);
}
