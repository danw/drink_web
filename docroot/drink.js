/*
 * Drink Web Interface
 *
 * Copyright 2008-2010 Dan Willemsen
 * Licensed under the MIT (MIT-LICENSE.txt) license
 */

// TODO: Replace with placeholder plugin
$.fn.extend({
    unfocusColor: function(str, unColor, color) {
        $(this).focus(function() {
            var self = $(this);
            if(this.value == str)
                self.val('');
            self.css('color', color);
        }).blur(function() {
            var self = $(this);
            if(this.value == '')
                self.css('color', unColor).val(str);
        }).each(function() {
            var self = $(this);
            if(this.value != str)
                self.css('color', color);
            else
                self.css('color', unColor);
        });
    }
});

$.fn.extend({
    collapsible: function(content) {
        var self = $(this);
        var hidden = undefined;
        self.click(function() {
            if (hidden === undefined)
                hidden = (content.css('display') == 'none');
            if (hidden)
            {
                self.removeClass('ui-corner-bottom ui-state-default').addClass('ui-state-active');
                content.show('slide', { direction: 'up' });
            }
            else
            {
                content.hide('slide', { direction: 'up', complete: function() {
                    self.removeClass('ui-state-active').addClass('ui-corner-bottom ui-state-default');
                }});
            }
            hidden = !hidden;
        });
    }
});

drink = {}

drink.websocket = new (function() {
    var self = this;
    this.use = false;
    this.ws = false;
    this.requests = {};
    var backoff_orig = 500;
    var backoff_limit = 10000;
    this.backoff_delay = backoff_orig;
    
    this.gotMessage = function(evt) {
        var data = JSON.parse(evt.data);
        drink.log(data);
        if ("event" in data) {
            drink.log("WS Got event: " + data.event, data.data);
            $('body').trigger(data.event + '_event', data.data);
        } else if("response" in data) {
            if (data.response in self.requests) {
                if (data.status == "error") {
                    drink.log("WS Got error: " + data.reason, data.data);
                    if ("error" in self.requests[data.response])
                        self.requests[data.response].error(data.reason, data.data);
                } else {
                    self.requests[data.response].success(data.data);
                }
                delete self.requests[data.response];
            } else {
                drink.log("Got a response to an unknown request!");
            }
        } else {
            drink.log("WS Unknown message");
        }
    }
    
    this.remoteCall = function(options) {
        if (this.ws === false) {
            drink.log("WTF, no open websocket, why am i being called");
            return;
        }
        request = { request: options.command, id: Math.floor(Math.random() * 10000000), args: options.args }
        self.requests[request.id] = options;
        self.ws.send(JSON.stringify(request));
    }
    
    this.init = function() {
        if (!("WebSocket" in window)) {
            drink.log("Upgrade to Chrome or another browser that support websockets!");
            return;
        }
        
        // TODO: dynamic websocket address
        self.ws = new WebSocket("ws://mini.danw.org:42080/drink/events");
        self.ws.onopen = function() {
            drink.log("WS Got open event");
            self.use = true;
            self.backoff_delay = backoff_orig;
        }
        self.ws.onmessage = self.gotMessage;
        self.ws.onclose = function() {
            drink.log("WS Got close event");
            self.use = false;
            self.ws = false;
            // TODO: go through self.requests and error all of them
            setTimeout(self.init, self.backoff_delay);
            self.backoff_delay *= 2;
            if (self.backoff_delay > backoff_limit) self.backoff_delay = backoff_limit;
        }
    }

    $(document).ready(function() { self.init(); });
})();

drink.remoteCall = function(options) {
    if (!("command" in options)) {
        drink.log("remoteCall without command: ");
        drink.log(options);
        return;
    }

    if(drink.websocket.use) {
        drink.log("RC Using Websocket");
        drink.websocket.remoteCall(options);
    } else {
        drink.log("RC Using Ajax");
        drink.ajax(options);
    }
}

drink.ajax = function(call_options) {
    var options = ("ajaxOptions" in call_options) ? call_options.ajaxOptions : {};

    options.url = "/drink/" + call_options.command;
    options.data = call_options.args;
    options.dataType = 'json';
    options.error = function() {
        drink.log("Error fetching " + options.url);
        if("error" in call_options)
            call_options.error(null);
    }
    options.success = function(data, status) {
        if(data.status == "error") {
            drink.log("Error returned from " + options.url + " - " + data.reason);
            if("error" in call_options)
                call_options.error(data.reason);
        } else {
            call_options.success(data.data);
        }
    }
    $.ajax(options);
}

drink.log = function(str) {
    if(window.console && console.log)
        console.log(str);
//    else
//        alert(str);
}

drink.time = {
    tz_offset: (new Date()).getTimezoneOffset() * 60,
    
    nowUTC: function() {
        return Math.floor((new Date()).getTime() / 1000);
    },
    
    fromUTC: function(val) {
        var time = new Date();
        time.setTime(val * 1000);
        return time;
    },
    
    today: function() {
        return new Date().toDateString();
    },
    
    yesterday: function() {
        var yesterday = new Date();
        yesterday.setTime(yesterday.getTime() - 86400000);
        return yesterday.toDateString();
    },
    
    prettyDateTime: function(t) {
        var timeStr = t.toDateString();
        
        if(timeStr == drink.time.today()) {
            return $.strftime("Today %H:%M:%S", t, false);
        } else if(timeStr == drink.time.yesterday()) {
            return $.strftime("Yesterday %H:%M:%S", t, false);
        }
        
        return $.strftime("%m/%d/%Y %H:%M:%S", t, false);
    }
}

$(document).ready(function() {
    var gotUser = function(data) {
        $('body').data('user', data).trigger('user_changed', data);
    };
    
    $('body').data('user', null);
    $('#header').hide();
    $('#tabs').hide();
    
    $('body').bind('user_refresh', function() {
        drink.remoteCall({
            command: 'currentuser',
            args: [],
            success: function(data) {
                $('body').data('user', data).trigger('user_changed', data);
            },
            error: function() {
                drink.log("Error getting current user");
                // TODO: deal with non-webauth logins?
                // Refresh the page, hopefully making the user re-webauth if necessary
                location.reload(true);
            }
        });
    });
    
    $('body').bind('user_changed', function(e, user) {
        $('#header').show();
        $('#tabs').show();
        $('#currentuser').text(user.username);
        $('#currentuser_balance').text(user.credits);
        if(user.admin)
            $('#currentuser_admin').show();
        else
            $('#currentuser_admin').hide();
    });

    $('body').bind('money_log_event', function(e, data) {
        var user = $('body').data('user');
        if (user.username != data.username) return;
        user.credits += data.amount;
        $('body').data('user', user).trigger('user_changed', user);
    });

    $('body').bind('user_changed_event', function(e, data) {
        var user = $('body').data('user');
        var changed = 0;
        if (user.username == data.username) {
            if ("admin" in data) {
                if (data.admin.old != data.admin["new"]) {
                    user.admin = data.admin["new"];
                    changed = 1;
                }
            }
            if (changed)
                $('body').data('user', user).trigger('user_changed', user);
        }

    });
});

drink.tab = new (function() {
    var self = this;
    var tab_elem;
    var anchors;
    var selectedTab;

    $(document).ready(function () {
        anchors = $('#tabs ul:first li:has(a[href])').map(function() { return $('a', this)[0] });
        tab_elem = $('#tabs').tabs( {
            //cookie: {expires: 7, path: '/', secure: true}, cookieName: 'main';
            selected: tabSelected,
            show: tabSelected // Only for this call
        });
        tab_elem.unbind('tabsshow', tabSelected);
        
        $('body').bind('user_changed', function(e, user) {
            for(var tab in drink.tabs) {
                if(drink.tabs[tab].admin_required && !user.admin) {
                    if(selectedTab == tab) {
                        // TODO: figure out first legit tab to select
                        tab_elem.tabs('select', 'drink_machines');
                    }

                    tab_elem.tabs('disable', tab);
                }
                else
                {
                    tab_elem.tabs('enable', tab);
                }
            }
        });
    });
    
    var tabSelected = function(e, ui) {
        var newTab = ui.tab.hash.slice(1, ui.tab.hash.length);
     
        if(drink.tabs[selectedTab] && drink.tabs[selectedTab].hide_tab && typeof drink.tabs[selectedTab].hide_tab == 'function')
            drink.tabs[selectedTab].hide_tab();
        if(drink.tabs[newTab] && drink.tabs[newTab].show_tab && typeof drink.tabs[newTab].show_tab == 'function')
            drink.tabs[newTab].show_tab();
     
        selectedTab = newTab;
 
        return true;
    }
    
    return this;
})();

drink.tabs = {}

drink.tabs.temperatures = new (function() {
    var self = this;

    var last_update = false;
    var refresh_interval = 120;
    
    var Length = 60 * 60 * 4; // 4 hours of data
    var MaxBreak = 120; // Break the graph if there is more than 2 minutes between data points
    var plot = null;
    var plot_data = null;
    var temp_data = null;
    var maximum_time = null;
        
    var gotTemps = function(data) {
        last_update = drink.time.nowUTC();
        plot_data = [];
       
        temp_data = data;
        maximum_time = data.start + data.length;

        /* Convert to local time */
        var local_start = data.start - drink.time.tz_offset;
        var local_length = data.length - drink.time.tz_offset;
        for(var m in data.machines)
            for(var i in data.machines[m])
                data.machines[m][i][0] = data.machines[m][i][0] - drink.time.tz_offset;
        
        var max_time = local_start + local_length - 60;

        for(var m in data.machines) {
            if(data.machines[m].length == 0)
                continue;

            var prev = data.machines[m][0][0];
            var temps = {data: []};
            for(var i in data.machines[m]) {
                var t = data.machines[m][i];
                
                if(prev + MaxBreak < t[0])
                    temps.data.push([(prev + MaxBreak) * 1000, null]);
                
                if(max_time < t[0])
                    max_time = t[0];
                
                prev = t[0];
                temps.data.push([t[0] * 1000, t[1]]);
            }
            
            if(m == 'littledrink')
                temps.label = "Little Drink";
            else if(m == 'bigdrink')
                temps.label = "Big Drink";
            else
                temps.label = m;

            plot_data.push(temps);
        }

        for (var m in data.machines)
            for (var i in data.machines[m])
                data.machines[m][i][0] = data.machines[m][i][0] + drink.time.tz_offset;
        
        plot = $.plot($('#temperature_plot'), plot_data,
            {xaxis: {mode: "time", min: local_start * 1000, max: max_time * 1000}});
    }
    
    var getTemps = function(From, Length) {
        drink.remoteCall({
            command: 'temperatures',
            args: {from: From, length: Length},
            success: gotTemps
        });
    }
    
    this.admin_required = false;
    
    this.show_tab = function() {
        if(last_update == false || last_update + refresh_interval < drink.time.nowUTC())
            self.refresh();
    }
    
    this.refresh = function() {
        getTemps(drink.time.nowUTC() - Length, Length + 60);
    }
   
    $(document).ready(function() {
        $('body').bind('temperature_event', function(e, data) {
            if (maximum_time == null) return;
            maximum_time = (maximum_time > data.time) ? maximum_time : data.time;
            temp_data.start = maximum_time - Length;
            temp_data.machines[data.machine][temp_data.machines[data.machine].length] = [data.time, data.temperature];
            gotTemps(temp_data);
        });
    });

    return this;
})();

drink.tabs.logs = new (function () {
    var self = this;
    
    var last_update = false;
    var refresh_interval = 60;
    
    var offset = 0;
    var limit = 20;

    var logElem = undefined;
    
    var gotLogs = function(data) {
        last_update = drink.time.nowUTC();
        
        if(data.start > 0)
            $('.logprev').show();
        else
            $('.logprev').hide();
        offset = data.start;
        $('.logoffset').html('' + offset);
        
        if(limit == data.lines.length)
            $('.lognext').show();
        else
            $('.lognext').hide();

        logElem = $('#logcontainer').empty();
        var lines = [];

        for(var i = 0; i < data.lines.length; i++) {
            var l = data.lines[i];

            lines[lines.length] = self.formatLog(l.type, l);
        }
        logElem.append(lines.join(''));
    }

    this.formatLog = function(type, data) {
        var time = drink.time.fromUTC(data.time);
        var d = drink.time.prettyDateTime(time);

        if (type == 'drop') {
            var error = data.status.search(/error/i) != -1;
            return [
                    '<tr', (error) ? ' class="error"' : '', '><td class="type">Drop</td><td class="time">', d,
                    '</td><td class="username">', data.username, 
                    '</td><td class="info">Dropped ', data.slot, ' from ', data.machine, '</td><td class="status">', data.status, '</td></tr>'
                ].join('');
        } else if(type == 'money') {
            var error = data.reason.search(/error/i) != -1;
            return [
                    '<tr', (error) ? ' class="error"' : '', '><td class="type">Money</td><td class="time">', d,
                    '</td><td class="username">', data.username,
                    '</td><td class="info">Admin: ', data.admin, ' Amount: ', data.amount, ' Direction: ', data.direction,
                    '</td><td class="reason">', data.reason, '</td></tr>'
                ].join('');

        }
    }

    this.admin_required = false;
    
    this.show_tab = function() {
        if(last_update == false || last_update + refresh_interval < drink.time.nowUTC())
            self.refresh();
    }

    this.refresh = function() {
        drink.remoteCall({
            command: 'logs', 
            args: {offset: offset, limit: limit}, 
            success: gotLogs
        });
    }
    
    $(document).ready(function() {
        logElem = $('#logcontainer');

        $('body').bind('user_changed', function(e, user) {
            last_update = false;
            // TODO fix
            if(drink.tabs.selectedTab == 'logs')
                self.refresh();
        });

        $('body').bind('money_log_event', function(e, data) {
            if (logElem == undefined) return;
            if (offset != 0) return;
            logElem.prepend(self.formatLog('money', data));
        });

        $('body').bind('drop_log_event', function(e, data) {
            if (logElem == undefined) return;
            if (offset != 0) return;
            logElem.prepend(self.formatLog('drop', data));
        });

        $('.logprev').click(function() {
            offset -= limit;
            offset = (offset > 0) ? offset : 0;
            self.refresh();
            return false;
        }).hide();
        
        $('.lognext').click(function() {
            offset += limit;
            self.refresh();
            return false;
        });
    });
    
    return this;
})();

drink.tabs.drink_machines = new (function() {
    var self = this;

    var last_update = false;
    var refresh_interval = 60;
    
    var machine_list = {};
    var machine_info = false;
    
    var pretty_available = function(count) {
        if(count == 0) {
            return 'Out';
        } else if(count == 1) {
            return 'Available';
        } else {
            return count;
        }
    }
    
    var Machine = function(machineList, info) {
        var self = this;
        var visible = false;
        self.slots = []
        var slots_dom = false;
        
        var Slot = function(machine, info) {
            var self = this;
           
            var slotDom = $('<tr><td class="slot_num"></td> \
                                 <td class="slot_name"></td> \
                                 <td class="slot_price"></td> \
                                 <td class="slot_available"></td> \
                                 <td class="slot_actions"> \
                                    <a href="#" class="slot_action_drop">Drop</a> \
                                    <a href="#" class="slot_action_edit">Edit</a> \
                                    <a href="#" class="slot_action_disable">Disable</a> \
                                    <a href="#" class="slot_action_delete">Delete</a> \
                                 </td></tr>');

            slotDom.find('.slot_action_drop').click(function() { self.drop(); return false; });
            slotDom.find('.slot_action_edit').click(function() { self.edit(); return false; });
            slotDom.find('.slot_action_disable').click(function() { self.disable(); return false; });
            slotDom.find('.slot_action_delete').click(function() { self.slot_delete(); return false; });

            this.updateInfo = function(info) {
                self.info = info;

                slotDom.find('.slot_num').text(info.num);
                slotDom.find('.slot_name').text(info.name);
                slotDom.find('.slot_price').text(info.price);
                slotDom.find('.slot_available').text(pretty_available(info.available));
                slotDom.find('.slot_action_disable').text(info.disabled ? "Enable" : "Disable" );

                self.userChanged();
            }

            this.userChanged = function() {
                var droppable = true;
                var userinfo = $('body').data('user');
                if (userinfo == null) return;
                
                if(!machine.connected) droppable = false;
                if(!self.info.available) droppable = false;
                if(self.info.disabled) droppable = false;
                if(userinfo.credits < self.info.price) droppable = false;
                
                if(droppable)
                    slotDom.find('.slot_action_drop').show();
                else
                    slotDom.find('.slot_action_drop').hide();
            }

            this.drop = function() {
                var delay = prompt("Delay? Enter for immediate");
                if (delay == null)
                    return; // Cancel
                if (delay == '')
                    delay = 0;
                else
                    delay = parseInt(delay);
                if (delay == NaN) {
                    alert("Invalid Delay");
                    return;
                }
                self.dropNow(delay);
            }

            this.dropNow = function(delay) {
                var delay = 0;
                if(arguments.length == 1) delay = arguments[0];

                drink.remoteCall({
                    command: 'drop',
                    args: { machine: machine.info.machineid, slot: self.info.num, delay: delay },
                    success: function() {
                        alert('Dropping... RUN!');
                    },
                    ajaxOptions: {
                        type: 'POST'
                    }
                });
            }

            this.edit = function() {
                var name = prompt("Name", self.info.name);
                if(name == null || name == '')
                    return;
                var price = prompt("Price", self.info.price);
                if(price == null || price == '')
                    return;
                var price = new Number(price);
                if(price == NaN || price < 0)
                    return;
                var available = prompt("Available", self.info.available);
                if(available == null || available == '')
                    return;
                var available = new Number(available);
                if(available == NaN || available < 0)
                    return;
                self.set_slot_info(name, price, available, self.info.disabled);
            }

            this.disable = function() {
                self.set_slot_info(self.info.name, self.info.price, self.info.available, !self.info.disabled);
            }

            this.set_slot_info = function(name, price, available, disabled) {
                drink.remoteCall({
                    command: 'setslot',
                    args: { machine: machine.info.machineid, slot: self.info.num, name: name,
                            price: price, available: available, disabled: disabled },
                    success: function() {},
                    error: function() {},
                    ajaxOptions: { type: 'POST' }
                });
            }

            this.slot_delete = function() {
                drink.remoteCall({
                    command: 'delslot',
                    args: { machine: machine.info.machineid, slot: self.info.num },
                    success: function() {},
                    error: function() {},
                    ajaxOptions: { type: 'POST' }
                });
            }

            this.remove = function() {
                slotDom.remove();
            }

            this.updateInfo(info);
            slots_dom.append(slotDom);
            self.dom = slotDom; 
            return this;
        }
        
        var machDom = $('<div class="machine_title ui-helper-reset ui-helper-clearfix ui-state-active ui-widget-header ui-corner-top"> \
                <a href="#" class="machine_remove" style="float:right" title="Delete">remove</a> \
                <a href="#" class="machine_edit" style="float:right" title="Edit">edit</a> \
                <span class="machine_title_span ui-helper-reset"></span></div> \
            <div class="machine_contents ui-helper-reset ui-widget-content ui-corner-bottom"> \
            <div class="machine_edit_form"></div>\
            <table><thead><tr><th>Slot Num</th><th>Name</th><th>Price</th><th>Available</th><th>Actions</th></tr></thead> \
            <tbody></tbody></table> \
            <a href="#" class="machine_add_slot">Add Slot</a></div>');
        
        var editDom = $('<form> \
            ID: <input type="text" class="machine_edit_id" disabled="disabled" /> Name: <input type="text" class="machine_edit_name" /> \
            Password: <input type="text" class="machine_edit_password" /><br /> \
            Public IP: <input type="text" class="machine_edit_public_ip" /> Machine IP: <input type="text" class="machine_edit_machine_ip" /><br /> \
            <input type="checkbox" class="machine_edit_available_sensor" value="true">Available Sensor</input> \
            <input type="checkbox" class="machine_edit_allow_connect" value="true">Allow Connect</input> \
            <input type="checkbox" class="machine_edit_admin_only" value="true">Admin Only</input><br /> \
            <input type="submit" value="Update Machine" /> \
            </form>');
        
        editDom.submit(modMachine);
        machDom.find('.machine_edit_form').append(editDom);
        machDom.find('.machine_edit_form').hide();
        slots_dom = machDom.find('tbody');
        machDom.filter('.machine_title').mouseover(function() { $(this).addClass('ui-state-hover') })
                                        .mouseout(function () { $(this).removeClass('ui-state-hover') })
                                        .collapsible(machDom.filter('.machine_contents'));
        machDom.find('.machine_remove').button({text: false, icons: { primary: 'ui-icon-trash' }})
        machDom.find('.machine_edit').button({text: false, icons: { primary: 'ui-icon-pencil' }}).click(function() {
            self.open();
            machDom.find('.machine_edit_form').toggle();
            return false;
        });
        machDom.find('.machine_remove').click(function() {
            $('<div title="Remove Machine?"><span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;"></span>Are you sure you want to delete "' + self.info.name + '"? This cannot be undone.</div>').dialog({
                resizable: false,
                height: 130,
                modal: true,
                dialogClass: 'ui-state-error',
                buttons: {
                    'Delete Machine': function() {
                        $(this).dialog('close')
                        delMachine(self.info.machineid);
                    },
                    Cancel: function() {
                        $(this).dialog('close')
                    }
                }
            });
            return false;
        });
        machDom.find('.machine_add_slot').click(function() {
            var num = prompt("Slot Num");
            if (num == null || num == '') return;
            num = new Number(num);
            if (num == NaN || num < 0) return;
            var name = prompt("Name");
            if (name == null || name == '') return;
            var price = prompt("Price");
            if (price == null || price == '') return;
            price = new Number(price);
            if (price == NaN || price < 0) return;
            var available = prompt("Available");
            if (available == null || available == '') return;
            available = new Number(available);
            if (available == NaN || available < 0) return;
            
            drink.remoteCall({
                command: 'addslot',
                args: { machine: self.info.machineid, slot: num, name: name,
                        price: price, available: available, disabled: true },
                success: function() {},
                error: function() {},
                ajaxOptions: { type: 'POST' }
            });

            return false;
        });
        
        this.add = function(list) {
            machineList.append(machDom);
            self.dom = machDom;
            visible = true;
        }
        
        this.remove = function(list) {
            machDom.remove();
            //machDom.hide('fade', function() { machDom.remove() });
            visible = false;
        }

        this.open = function() {
            if (machDom.filter('.machine_title').hasClass('ui-state-active')) return;
            //machDom.filter('.machine_contents').show();
            machDom.filter('.machine_title').click().removeClass('ui-corner-bottom');
        }

        this.close = function() {
            if (!machDom.filter('.machine_title').hasClass('ui-state-active')) return;
            //machDom.filter('.machine_contents').hide();
            machDom.filter('.machine_title').click().addClass('ui-corner-bottom');
        }

        this.connected = function() {
            if (self.info.connected) return;
            self.info.connected = true;
            this.open();
        }

        this.disconnected = function() {
            if (!self.info.connected) return;
            self.info.connected = false;
            this.close();
        }

        this.updateInfo = function(info) {
            self.info = info;

            machDom.find('.machine_title_span').text(info.name);
            
            editDom.find('.machine_edit_id').val(info.machineid);
            editDom.find('.machine_edit_name').val(info.name);
            editDom.find('.machine_edit_password').val(info.password);
            editDom.find('.machine_edit_public_ip').val(info.public_ip);
            editDom.find('.machine_edit_machine_ip').val(info.machine_ip);
            editDom.find('.machine_edit_available_sensor').attr('checked', info.available_sensor);
            editDom.find('.machine_edit_allow_connect').attr('checked', info.allow_connect);
            editDom.find('.machine_edit_admin_only').attr('checked', info.admin_only);
            
            for(var slotnum in info.slots) {
                if (slotnum in self.slots) {
                    self.slots[slotnum].updateInfo(info.slots[slotnum]);
                } else {
                    self.slots[slotnum] = new Slot(self, info.slots[slotnum]);
                }
            }
        }

        this.slot_added = function(slot) {
            self.slots[slot.num] = new Slot(self, slot);
        }

        this.updateInfo(info);
        if(!info.connected) this.close();
        this.add(machineList);

        return this;
    }
    
    var machine_add_dom = function() {
        var me = $('<div id="machine_add_div" class="ui-helper-reset ui-widget-content ui-helper-hidden ui-corner-bottom"> \
            <form id="machine_add_form"> \
            ID: <input type="text" class="machine_add_id" /> Name: <input type="text" class="machine_add_name" /> \
            Password: <input type="text" class="machine_add_password" /><br /> \
            Public IP: <input type="text" class="machine_add_public_ip" /> Machine IP: <input type="text" class="machine_add_machine_ip" /><br /> \
            <input type="checkbox" class="machine_add_available_sensor" checked="checked">Available Sensor</input> \
            <input type="checkbox" class="machine_add_allow_connect" checked="checked">Allow Connect</input> \
            <input type="checkbox" class="machine_add_admin_only" checked="checked">Admin Only</input><br /> \
            <input type="submit" value="Add Machine &raquo;" /> \
            </form></div>');

        me.find('form').submit(addMachine);
        return me;
    }
    
    var gotMachines = function(data) {
        last_update = drink.time.nowUTC();
        
        machine_info = data;
        var machinelist = $('#machines');
        
        for(var machine in data) {
            if (machine in machine_list)
            {
                machine_list[machine].updateInfo(data[machine])
            }
            else
            {
                machine_list[machine] = new Machine(machinelist, data[machine]);
            }
        }
        for(var machine in machine_list) {
            if (!(machine in data)) {
                machine_list[machine].remove();
                delete machine_list[machine];
            }
        }
        
        self.user_update();
    }
       
    var addMachine = function() {
        drink.remoteCall({
            command: 'addmachine',
            args: {
                machine: $('.machine_add_id').val(),
                password: $('.machine_add_password').val(),
                name: $('.machine_add_name').val(),
                public_ip: $('.machine_add_public_ip').val(),
                available_sensor: $('.machine_add_available_sensor').attr('checked'),
                machine_ip: $('.machine_add_machine_ip').val(),
                allow_connect: $('.machine_add_allow_connect').attr('checked'),
                admin_only: $('.machine_add_admin_only').attr('checked')
            },
            success: function() {
                $('#machine_add_link').click();
                $('#machine_add_form input[type=text]').val("");
                $('#machine_add_form input[type=checkbox]').attr('checked', true);
                //self.refresh();
            },
            error: function(reason, data) {
                // TODO: handle invalid_arg
                if (reason == "invalid_arg")
                    alert("Invalid argument: " + data.arg);
                else
                    alert("Error adding machine: " + reason);
            },
            ajaxOptions: {
                type: 'POST'
            }
        });
        return false;
    }

    var modMachine = function() {
        drink.log("Mod machine...");
        var self = this;
        drink.remoteCall({
            command: 'modmachine',
            args: {
                machine: $(self).find('.machine_edit_id').val(),
                password: $(self).find('.machine_edit_password').val(),
                name: $(self).find('.machine_edit_name').val(),
                public_ip: $(self).find('.machine_edit_public_ip').val(),
                available_sensor: $(self).find('.machine_edit_available_sensor').attr('checked'),
                machine_ip: $(self).find('.machine_edit_machine_ip').val(),
                allow_connect: $(self).find('.machine_edit_allow_connect').attr('checked'),
                admin_only: $(self).find('.machine_edit_admin_only').attr('checked')
            },
            success: function() {},
            error: function(reason, data) {
                // TODO: handle invalid_arg
                alert("Error modding machine");
            },
            ajaxOptions: {
                type: 'POST'
            }
        });
        return false;
    }

    var delMachine = function(machine) {
        drink.log("Del machine " + machine);
        drink.remoteCall({
            command: 'delmachine',
            args: {
                machine: machine
            },
            success: function() {
                /* bye */
            },
            error: function() {
                alert("Error deleting machine");
            },
            ajaxOptions: {
                type: 'POST'
            }
        });
    }

    this.admin_required = false;
    
    this.show_tab = function() {
        if(last_update == false || last_update + refresh_interval < drink.time.nowUTC())
            self.refresh();
    }
    
    $(document).ready(function () {
        $('body').bind('machine_added_event', function(e, machine) {
            machine_list[machine.machineid] = new Machine($('#machines'), machine);
            self.user_update();
        });

        $('body').bind('machine_modified_event', function(e, machine) {
            drink.log(machine);
            if (machine.machineid in machine_list) {
                machine_list[machine.machineid].updateInfo(machine);
            } else {
                machine_list[machine.machineid] = new Machine($('#machines'), machine);
                self.user_update();
            }
        });

        $('body').bind('machine_deleted_event', function(e, machine) {
            if (machine.machineid in machine_list) {
                machine_list[machine.machineid].remove();
                delete machine_list[machine.machineid];
            }
        });

        $('body').bind('machine_connected_event', function(e, machine) {
            if (machine.machineid in machine_list) {
                machine_list[machine.machineid].connected();
            }
        });

        $('body').bind('machine_disconnected_event', function(e, machine) {
            if (machine.machineid in machine_list) {
                machine_list[machine.machineid].disconnected();
            }
        });

        $('body').bind('slot_added_event', function(e, data) {
            if (data.machineid in machine_list) {
                if (data.slot.num in machine_list[data.machineid].slots) {
                    drink.log("Slot added that we already know about");
                } else {
                    machine_list[data.machineid].slot_added(data.slot);
                }
            } else {
                drink.log("Slot added on machine that we don't know");
            }
        });

        $('body').bind('slot_modified_event', function(e, data) {
            if (data.machineid in machine_list) {
                if (data.slot.num in machine_list[data.machineid].slots) {
                    machine_list[data.machineid].slots[data.slot.num].updateInfo(data.slot);
                } else {
                    drink.log("Slot modified that we don't know about");
                }
            } else {
                drink.log("Slot modified on machine that we don't know");
            }
        });

        $('body').bind('slot_deleted_event', function(e, data) {
            if (data.machineid in machine_list) {
                if (data.slot in machine_list[data.machineid].slots) {
                    machine_list[data.machineid].slots[data.slot].remove();
                } else {
                    drink.log("Slot deleted that we don't know about");
                }
            } else {
                drink.log("Slot deleted on machine that we don't know");
            }
        });
    });
    
    this.user_update = function() {
        var admin = $('#drink_machines .slot_action_edit, #drink_machines .slot_action_disable, #drink_machines .slot_action_delete, #drink_machines .machine_edit, #drink_machines .machine_remove, #machine_add_link, #drink_machines .machine_add_slot');
        
        var userinfo = $('body').data('user');
        if (userinfo == null) return;

        for (machineid in machine_list) {
            for (slot in machine_list[machineid].slots) {
                machine_list[machineid].slots[slot].userChanged(userinfo);
            }
        }

        if(userinfo.admin) {
            admin.show();
        } else
            admin.hide();
    }
    
    this.refresh = function() {
        drink.remoteCall({
            command: 'machines', 
            args: [], 
            success: gotMachines
        });
    }
    
    $(document).ready(function() {
        var m_a_link = $('<span class="ui-helper-reset ui-state-default ui-widget-header ui-corner-top ui-corner-bottom" id="machine_add_link">Add Machine</span>');
        var m_a_dom = machine_add_dom();
        $('#machines_holder').append(m_a_link).append(m_a_dom);
        m_a_link.mouseover(function() { m_a_link.addClass('ui-state-hover') })
                .mouseout(function () { m_a_link.removeClass('ui-state-hover') }).collapsible(m_a_dom);

        $('body').bind('user_changed', function(e, data) { self.user_update() });

        self.user_update();
    });
    
    return this;
})();

drink.tabs.user_admin = new (function() {
    var self = this;
    
    var last_update = false;
    var current_edit_user = null;
    
    var get_user_info = function() {
        var username = $('#user_admin_username').val();
        if(username == 'username' || username == '')
            return false;
        
        drink.remoteCall({
            command: 'userinfo', 
            args: {user: username}, 
            success: got_user_info,
        });

        return false;
    }

    var got_user_info = function(userinfo) {
        current_edit_user = userinfo;

        $('#user_admin_user_username').text(current_edit_user.username);
        $('#user_admin_user_credits').text(current_edit_user.credits);
        $('#user_admin_user_admin').text(current_edit_user.admin);
        var ibuttons = $('#user_admin_user_ibuttons').empty();
        $.each(current_edit_user.ibuttons, function(n, ibutton) {
            var i = $('<li><span class="ibutton"></span> <a href="#">X</a></li>').appendTo(ibuttons).data("ibutton", ibutton);
            i.find('.ibutton').text(ibutton);
            i.find('a').click(removeiButton);
        });
        
        $('#user_admin > table').show();
    }

    var addiButton = function() {
        if(current_edit_user == null)
            return;
        var ibutton = prompt("Enter iButton:");
        if(ibutton == '' || ibutton == null)
            return;
        mod_user(current_edit_user.username, "addibutton", ibutton, '');
        
        return false;
    }

    var removeiButton = function() {
        if(current_edit_user == null)
            return;
        
        var ibutton = $(this).parents('li').eq(0).data("ibutton");
        if(confirm("Are you sure you want to delete: " + ibutton))
            mod_user(current_edit_user.username, "delibutton", ibutton, '');
        
        return false;
    }

    var modcredits_reason_change = function() {
        var reason = $('#user_admin_mod_reason');
        var credits = $('#user_admin_mod_credits');
        if(reason.val() == 'fix_amount' && credits.val() == '') {
            credits.val(current_edit_user.credits);
        }
        if(reason.val() == 'add_money' && credits.val() == '' + current_edit_user.credits) {
            credits.val('');
        }
    }

    var modcredits = function() {
        var diff = parseInt($('#user_admin_mod_credits').val());
        if(diff == NaN) {
            alert("Not a Number!");
            return;
        }
        var reason = $('#user_admin_mod_reason').val();
        if(reason == 'other') {
            while(reason == 'other' || reason == '')
                reason = prompt("Please enter reason: (lower case with underscores)");
            if(reason == null)
                return;
            if(!confirm("Press OK if the value is the difference from their current balance, Cancel if it's the full value.")) {
                diff = diff - current_edit_user.credits;
            }
        } else if(reason == 'fix_amount') {
            diff = diff - current_edit_user.credits;
        }
        if(diff == 0)
            return;
        mod_user(current_edit_user.username, "modcredits", diff, reason);
        
        return false;
    }

    var toggle_admin = function() {
        if(current_edit_user == null)
            return;
        mod_user(current_edit_user.username, "admin", !current_edit_user.admin, '');
        
        return false;
    }

    var mod_user = function(username, attr, value, reason) {
        $('#user_admin_mod_form a').empty();
        $('#user_admin_mod_form form').empty();

        drink.remoteCall({
            command: 'moduser',
            args: { username: username, attr: attr, value: value, reason: reason },
            success: function() {},
            ajaxOptions: {
                type: 'POST'
            }
        });
    }
    
    this.admin_required = true;
    
    $(document).ready(function() {
        $('#user_admin_username').unfocusColor('username', 'gray', 'black');
        
        $('#user_admin_get_form').submit(get_user_info);
        $('#user_admin_mod_credits_form').submit(modcredits);
        $('#user_admin_add_ibutton').click(addiButton);
        $('#user_admin_toggle_admin').click(toggle_admin);
        $('#user_admin_mod_reason').change(modcredits_reason_change);
        $('#user_admin > table').hide();

        $('body').bind('user_changed_event', function(e, data) {
            if (current_edit_user == null || current_edit_user.username != data.username) return;
            var changed = false;
            if ("admin" in data) {
                current_edit_user.admin = data.admin["new"];
                changed = true;
            }
            if ("add_ibutton" in data) {
                current_edit_user.ibuttons.push(data.add_ibutton);
                changed = true;
            }
            if ("del_ibutton" in data) {
                current_edit_user.ibuttons = current_edit_user.ibuttons.filter(function(i) {
                    return (i != data.del_ibutton);
                });
                changed = true;
            }

            if (changed) got_user_info(current_edit_user);
        });

        $('body').bind('money_log_event', function(e, data) {
            if (current_edit_user == null || current_edit_user.username != data.username) return;
            current_edit_user.credits += data.amount;

            got_user_info(current_edit_user);
        });
    });
    
    return this;
})();

drink.tabs.connections = new (function() {
    var self = this;
    var listDom = undefined;
    var connections = {};
    var have_list = false;

    var Connection = function(data) {
        var self = this;
        var dom = undefined;
        var connection_data = undefined;

        this.updateInfo = function(data) {
            if (dom == undefined) return;
            dom.find('.connection_pid').text(data.pid);
            dom.find('.connection_user').text(data.username);
            dom.find('.connection_transport').text(data.transport);
            dom.find('.connection_app').text(data.app);

            connection_data = data;
        }

        this.disconnected = function() {
            self.remove();
        }

        this.remove = function() {
            if (dom == undefined) return;
            dom.remove();
        }

        dom = $('<tr><td class="connection_pid"></td><td class="connection_user"></td> \
                 <td class="connection_transport"></td><td class="connection_app"></td></tr>');
        this.updateInfo(data);
        listDom.append(dom);

        return this;
    };

    this.admin_required = false;

    this.user_update = function() {
    }

    this.show_tab = function() {
        if (have_list) return;
        have_list = true;
        drink.remoteCall({
            command: 'getconnections',
            args: {},
            success: function(data) {
                for(pid in connections)
                    connections[pid].remove();
                for(pid in data) {
                    connections[pid] = new Connection(data[pid]);
                }
            },
            ajaxOptions: { type: 'GET' }
        });
    }

    $(document).ready(function() {
        listDom = $('#connection_list');

        $('body').bind('connected_event', function(e, data) {
            if (data.pid in connections) return;
            connections[data.pid] = new Connection(data);
        });

        $('body').bind('disconnected_event', function(e, data) {
            if (!(data.pid in connections)) return;
            connections[data.pid].disconnected();
            delete(connections[data.pid]);
        });
    });
})();

$(document).ready(function() {
    $('body').trigger('user_refresh');
});

