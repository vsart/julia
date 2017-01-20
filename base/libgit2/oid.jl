# This file is a part of Julia. License is MIT: http://julialang.org/license

GitHash(id::GitHash) = id
GitHash(ptr::Ptr{GitHash}) = unsafe_load(ptr)::GitHash

function GitHash(ptr::Ptr{UInt8})
    if ptr == C_NULL
        throw(ArgumentError("NULL pointer passed to GitHash() constructor"))
    end
    oid_ptr = Ref(GitHash())
    ccall((:git_oid_fromraw, :libgit2), Void, (Ptr{GitHash}, Ptr{UInt8}), oid_ptr, ptr)
    return oid_ptr[]
end

function GitHash(id::Array{UInt8,1})
    if length(id) != OID_RAWSZ
        throw(ArgumentError("invalid raw buffer size"))
    else
        return GitHash(pointer(id))
    end
end

function GitHash(id::AbstractString)
    bstr    = String(id)
    len     = sizeof(bstr)
    oid_ptr = Ref(GitHash())
    err     = 0
    if len < OID_HEXSZ
        err = ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{UInt8}, Csize_t), oid_ptr, bstr, len)
    else
        err = ccall((:git_oid_fromstrp, :libgit2), Cint,
              (Ptr{GitHash}, Cstring), oid_ptr, bstr)
    end
    if err != 0
        return GitHash()
    else
        return oid_ptr[]
    end
end

function GitHash(ref::GitReference)
    if isempty(ref) || reftype(ref) != Consts.REF_OID
        return GitHash()
    end
    oid_ptr = ccall((:git_reference_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    if oid_ptr == C_NULL
        return GitHash()
    else
        return GitHash(oid_ptr)
    end
end

function GitHash(repo::GitRepo, ref_name::AbstractString)
    isempty(repo) && return GitHash()
    oid_ptr  = Ref(GitHash())
    @check ccall((:git_reference_name_to_id, :libgit2), Cint,
                    (Ptr{GitHash}, Ptr{Void}, Cstring),
                     oid_ptr, repo.ptr, ref_name)
    return oid_ptr[]
end

function GitHash(obj::Ptr{Void})
    oid_ptr = ccall((:git_object_id, :libgit2), Ptr{UInt8}, (Ptr{Void},), obj)
    if oid_ptr == C_NULL
        return GitHash()
    else
        return GitHash(oid_ptr)
    end
end

GitHash{T<:GitObject}(obj::T) = GitHash(obj === nothing ? obj : obj.ptr)

Base.hex(id::GitHash) = join([hex(i,2) for i in id.val])

raw(id::GitHash) = collect(id.val)

Base.string(id::GitHash) = hex(id)

Base.show(io::IO, id::GitHash) = print(io, "GitHash($(string(id)))")

Base.hash(id::GitHash, h::UInt) = hash(id.val, h)

cmp(id1::GitHash, id2::GitHash) = Int(ccall((:git_oid_cmp, :libgit2), Cint,
                                    (Ptr{GitHash}, Ptr{GitHash}), Ref(id1), Ref(id2)))

==(id1::GitHash, id2::GitHash) = cmp(id1, id2) == 0
Base.isless(id1::GitHash, id2::GitHash)  = cmp(id1, id2) < 0

iszero(id::GitHash) = !any(id.val[i] != zero(UInt8) for i in 1:OID_RAWSZ)

Base.zero(::Type{GitHash}) = GitHash()
