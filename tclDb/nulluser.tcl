package provide nulluser 1.0

# a helper class which implements minimal user information
# write a new one to give some semantics to the uid/gid fields
# then pass it to mkvfs as -user $obj
# mkvfs will handle {con,de}struction only of nulluser
snit::type nulluser {
    # map user id to user name
    method uid2user {uid} {
	if {$uid == 1000} {
	    return $::tcl_platform(user)
	} else {
	    return $uid
	}
    }

    # map user name to user id
    method user2uid {user} {
	if {$user eq $::tcl_platform(user)} {
	    #return 0
	    return 1000
	} else {
	    return $user
	}
    }

    # current/operative user name
    method user {} {
	return $::tcl_platform(user)
    }

    # current/operative user id
    method uid {} {
	return [$self user2uid [$self user]]
    }

    # map group id to group name
    method gid2group {gid} {
	if {$gid == 1000} {
	    return $::tcl_platform(user)
	} else {
	    return $gid
	}
    }

    # map group name to group id
    method group2gid {group} {
	if {$group eq $::tcl_platform(user)} {
	    #return 0
	    return 1000
	} else {
	    return $group
	}
    }

    # current/operative group name
    method group {} {
	return $::tcl_platform(user)
    }

    # current operative group id
    method gid {} {
	return [$self group2gid [$self group]]
    }

    # is uid in the group gid?
    method ingroup {gid uid} {
	return 0
    }
}
