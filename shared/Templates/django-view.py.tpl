# -*- coding: utf-8 -*-

# $Id:$

from django import forms
from django.contrib.auth.decorators import login_required
from django.contrib.auth.decorators import permission_required
from django.core.urlresolvers import reverse
from django.forms import widgets
from django.shortcuts import render_to_response
from django.shortcuts import redirect
from django.template import RequestContext
from django.utils.translation import ugettext_lazy as _

from bdfcee.homer import models as app_models


def (>>>POINT<<<)(request) :
    return render_to_response(
        "template_path",
        {},
        context_instance=RequestContext(request)
        )
